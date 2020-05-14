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

:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(main), [argv_options/3]).

:- use_module(h_api).
:- use_module(h_realtime).
:- use_module(security).
:- use_module(annotate).
:- use_module(h_config).
:- use_module(bot).

:- initialization(go, main).

go :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _RestArgv, Options),
    debug(h(init)),
    debug(h(rt(reply))),
    debug(h(rt(annotation))),
    load_config(Options),
    setting(hypothesis:group, Group),
    hrt_listen([group(Group)]),
    set_prolog_flag(toplevel_goal, prolog). % become interactive

%!  a
%
%   Creates simple annotation

a :-
    annotate('https://www.swi-prolog.org/pldoc/doc_for?object=assert/1',
             text_quote("Assert a ", "clause", " (fact or rule)"),
             "A clause is a rule in a predicate",
             Id,
             [ group(prolog)
             ]),
    format('Created annotation with id ~q~n', [Id]).
