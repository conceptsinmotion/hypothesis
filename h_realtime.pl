/*  Part of SWI-Prolog interface to Hypothesis.is

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, CWI, Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(h_realtime,
          [ hrt_listen/1,               % +Options
            hrt_whoami/1                % -Identity
          ]).
:- use_module(library(http/websocket)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).
:- use_module(library(option)).

:- use_module(h_query).

:- dynamic ( connection/1,              % WebSocket
             waiting/2,                 % Id, Thread
             config_message/2,          % Type, Messages
             client_id_cache/1          % ClientId
           ) as volatile.

/** <module>

From `filter.py`:

SCHEMA = {
    "type": "object",
    "properties": {
        # Ignored, but kept here for backwards compatibility.
        "match_policy": {"type": "string", "enum": ["include_any"]},
        # Ignored, but kept here for backwards compatibility.
        "actions": {
            "create": {"type": "boolean", "default": True},
            "update": {"type": "boolean", "default": True},
            "delete": {"type": "boolean", "default": True},
        },
        "clauses": {
            "type": "array",
            "items": {
                "field": {"type": "string", "format": "json-pointer"},
                "operator": {"type": "string", "enum": ["equals", "one_of"]},
                "value": "object",
            },
        },
    },
    "required": ["match_policy", "clauses", "actions"],
}

Example from sidebar

  {"filter":
    {"match_policy":"include_any",
     "clauses":[ {"field":"/uri",
                  "operator":"one_of",
                  "value":["https://www.swi-prolog.org/pldoc/doc_for?object=assert/1"
                          ],
                  "case_sensitive":false}
               ],
     "actions":{"create":true,"update":true,"delete":true}
    }
  }

{"messageType":"client_id","value":"adaa11cd571f21be91bc1a49166295ba"}

*/

%!  hrt_listen(+Options)
%
%   Listen to Hypothesis realtime events.

hrt_listen(_) :-
    connection(_),                      % update config?
    !.
hrt_listen(Options) :-
    hrt_config(Options),
    hrt_connect(WebSocket),
    asserta(connection(WebSocket)),
    thread_create(hrt_dispatch(WebSocket), _,
                  [ alias(hypothesis_listener)
                  ]),
    send_config.

hrt_connect(WebSocket) :-
    setting(hypothesis:api_key, APIKey),
    http_open_websocket('wss://hypothes.is/ws', WebSocket,
                        [ authorization(bearer(APIKey))
                        ]).

hrt_dispatch(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    debug(h(rt(receive)), 'JSON: message: ~p', [Message]),
    (   dispatch(Message)
    ->  true
    ;   print_message(warning, h(rt(dispatch_failed, Message)))
    ),
    hrt_dispatch(WebSocket).

dispatch(Message) :-
    _{opcode:pong} :< Message,
    !.
dispatch(Message) :-
    _{data:Data, format:json} :< Message,
    debug(h(rt(json)), 'JSON: message: ~p', [Data]),
    dispatch_json(Data).

dispatch_json(Data) :-
    _{ok:_, reply_to:Id} :< Data,                    % is a reply
    !,
    (   waiting(Id, Thread)
    ->  debug(h(rt(reply)), 'Reply: ~p to ~p', [Data, Thread]),
        thread_send_message(Thread, Data)
    ;   debug(h(rt(noreply)), 'Nobody waiting for ~p', [Id])
    ).
dispatch_json(Message) :-
    _{ type: "annotation-notification",
       options: Options,
       payload: Payload
     } :< Message,
     !,
     debug(h(rt(annotation)), 'Annotation event: ~p', [Message]),
     dispatch_annotation_notification(Options, Payload).
dispatch_json(Message) :-
    debug(h(rt), 'Dispatching ~p', [Message]),
    catch_with_backtrace(
        broadcast(hypothesis(event(Message))),
        Error,
        print_message(error, Error)).

dispatch_annotation_notification(Options, Payload) :-
    Options.get(action) == "delete",
    maplist(obj_anot_id, Payload, Annotations),
    broadcast(h(deleted_annotations, Annotations)).
dispatch_annotation_notification(Options, Payload) :-
    Options.get(action) == "create",
    broadcast(h(created_annotations, Payload)).

obj_anot_id(Obj, Id) :-
    Id = Obj.get(id).


		 /*******************************
		 *        CONFIGURATION		*
		 *******************************/

send_config :-
    connection(WebSocket),
    forall(config_message(_Type, Message),
           ws_send(WebSocket, json(Message))).

%!  hrt_config(+Options)
%
%   Prepare the configuration. This sets  up   the  client id and filter
%   clauses.

hrt_config(Options) :-
    config_client_id(Options),
    forall(config_filter(Options), true).

config_client_id(_Options) :-
    client_id(Id),
    debug(h(init), 'ClientID = ~p', Id),
    assertz(config_message(client_id,
                           _{messageType: client_id,
                             value:Id})).

config_filter(Options) :-
    option(group(GroupName), Options),
    h_user(_User, GroupId, [group(GroupName)]),
    debug(h(init), 'Filtering on group ~p (~p)', [GroupName, GroupId]),
    assertz(config_message(filter,
                           _{filter:_{ match_policy: include_any,
                                       actions: _{ create:true,
                                                   update:true,
                                                   delete:true
                                                 },
                                       clauses:
                                       [ _{ field: '/group',
                                            operator: one_of,
                                            case_sensitive: false,
                                            value: [ GroupId ]
                                          }
                                       ]
                                    }})).


%!  client_id(-Id)
%
%   Get a (random) client id. We can use this to reconnect after we lost
%   the connection.

client_id(Id) :-
    client_id_cache(Id0),
    !,
    Id = Id0.
client_id(Id) :-
    gen_client_id(Id),
    asserta(client_id_cache(Id)),
    !.

gen_client_id(Id) :-
    A is random(1<<(32*4)),
    format(atom(Id), '~16r', [A]).

%!  request_id(-RequestId)
%
%   Request IDs must be unique numbers. For   now we just count. I don't
%   know whether there is a maximum and we should round trip.

request_id(ReqId) :-
    flag(hrt_request_id, ReqId, ReqId+1).

%!  hrt_whoami(-Identity)
%
%   Fetch the user identity via the realtime websocket connection.
%
%   @arg Identity

hrt_whoami(Me) :-
    connection(WebSocket),
    thread_self(Self),
    request_id(ReqId),
    asserta(waiting(ReqId, Self), Ref),
    ws_send(WebSocket, json(_{id:ReqId, type:whoami})),
    (   thread_get_message(Self, Msg, [timeout(10)])
    ->  erase(Ref),
        atom_string(Me, Msg.userid)
    ;   erase(Ref),
        throw(time_limit_exceeded)
    ).
