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

:- module(h_config,
          [ load_config/1,              % +Options
            set_api_key/1               % +String
          ]).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(readutil)).
:- use_module(library(settings)).

:- setting(hypothesis:group, atom, '__world__',
           "Group to watch for events").

%!  load_config(+Options) is det.
%
%   Load or initialise the configuration.

load_config(Options) :-
    option(config(Config), Options, 'h.conf'),
    (   exists_file(Config)
    ->  debug(h(init), 'Loading settings from ~p', [Config]),
        load_settings(Config)
    ;   debug(h(init), 'No config file ~p, asking config', [Config]),
        ask('Please give the Hypothesis API key for the target user~n',
            'api key? ', string, Key),
        ask('Please give group to watch~n',
            'group? ', atom, Group),
        set_setting(hypothesis:api_key, Key),
        set_setting(hypothesis:group, Group),
        save_settings(Config)
    ).

ask(Question, Prompt, Type, Value) :-
    format(Question),
    setup_call_cleanup(
        prompt(Old, Prompt),
        read_line_to_string(current_input, String),
        prompt(_, Old)),
    to_type(Type, String, Value).

to_type(string,  String, String).
to_type(atom,    String, Atom)   :- atom_string(Atom, String).
to_type(number,  String, Number) :- number_string(Number, String).

%!  set_api_key(+String)
%
%   Set the Hypothesis API key to authenticate the user.

set_api_key(String) :-
    set_setting(hypothesis:api_key, String),
    save_settings.

