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

:- module(annotate,
          [ annotate/5,                 % +URL,+Selector,+Text,Id,+Options
            reply/4,                    % +Id,+Text,-ReplyId,+Options
            delete_annotation/1         % +Id
          ]).
:- use_module(library(option)).
:- use_module(h_api).
:- use_module(h_query).

%!  annotate(+URL, +Selector, +Text, -Id, +Options)
%
%   Create an a new annotation on URL.  Selector is one of
%
%     - text_quote(Prefix, Text, Suffix)

annotate(URL, Selector, Text, Id, Options) :-
    h_user(UserId, GroupId, Options),
    option(tags(Tags), Options, []),
    option(title(Title), Options, Text),
    selector_object(Selector, SelObject),
    permission_object(Permissions, Options),
    xsd_now(Now),
    Annot = _{ uri:URL,
               group:GroupId,
               document: _{title:Title},
               text: Text,
               tags: Tags,
               permissions: Permissions,
               target: [ _{ selector: [ SelObject ],
                            source: URL
                          }
                       ],
               updated: Now,
               user: UserId
             },
    create_annotation(Annot, Reply),
    atom_string(Id, Reply.id).

%!  reply(+Id, +Text, -ReplyId, +Options)
%
%   Create an annotation in reply to Id.

reply(Id, Text, ReplyId, Options) :-
    h_user(UserId, _GroupId, Options),
    h_annotation(Id, ReplyTo),
    URL = ReplyTo.uri,
    option(tags(Tags), Options, []),
    option(title(Title), Options, Text),
    permission_object(Permissions, Options),
    xsd_now(Now),
    Annot = _{ uri:URL,
               document: _{title:Title},
               text: Text,
               tags: Tags,
               permissions: Permissions,
               updated: Now,
               user: UserId,
               references:[Id]
             },
    create_annotation(Annot, Reply),
    atom_string(ReplyId, Reply.id).


selector_object(text_quote(Prefix, Text, Suffix),
                _{ exact: Text,
                   prefix: Prefix,
                   suffix: Suffix,
                   type: 'TextQuoteSelector'
                 }).

permission_object(_{ admin: [UserId],
                     delete: [UserId],
                     read: [Group],
                     update: [UserId]
                   },
                  Options) :-
    h_user(UserId, GroupId, Options),
    atom_concat('group:', GroupId, Group).

xsd_now(Now) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, 'UTC'),
    to_xsd(DateTime, XSDDateTime),
    xsd_time_string(XSDDateTime, _, Now).

to_xsd(date(Y,M,D,H,Mi,S,_,'UTC',_),
       date_time(Y,M,D,H,Mi,S,0)).

%!  delete_annotation(+Id)
%
%   Delete a single annotation

delete_annotation(Id) :-
    delete_annotation(Id, Reply),
    (   _{deleted:true} :< Reply
    ->  true
    ;   unexpected_reply(Reply)
    ).


unexpected_reply(Reply) :-
    throw(error(unexpected_reply(Reply),_)).
