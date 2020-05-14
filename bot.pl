:- module(bot,
          []).
:- use_module(library(broadcast)).
:- use_module(library(apply)).
:- use_module(library(porter_stem)).
:- use_module(library(xpath)).
:- use_module(library(aggregate)).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(debug)).
:- use_module(library(lists)).

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

:- use_module(annotate).

:- initialization listen(h(created_annotations, List),
                         maplist(created, List)).

created(Annotation) :-
    question(Annotation.text, Action),
    call(Action, Annotation).

question(Text, Action) :-
    tokenize_atom(Text, Tokens),
    maplist(downcase_atom, Tokens, LwrTokens),
    phrase(react(Action), LwrTokens),
    debug(bot(question), 'Semantics: ~p', [Action]).

react(reply(count(Word))) -->
    [ how ], often, [ is ], quoted(Word), used, [?].

often --> [often].
often --> [many, times].

quoted(Word) -->
    [ '"', Word, '"' ].
quoted(Word) -->
    [ '\'', Word, '\'' ].
quoted(Word) -->
    [ Word ].

used --> [used].
used --> [mentioned].


		 /*******************************
		 *           ACTIONS		*
		 *******************************/

reply(Question, Annotation) :-
    call(Question, Annotation, Answer),
    reply(Annotation.id, Answer, ReplyId,
          [ tags([answer]),
            title('Bot replied')
          ]),
    debug(bot(reply), 'Created annotation ~q', [ReplyId]).

count(Word, Annotation, Answer) :-
    count_word_in_uri(Annotation.uri, Word, Count),
    format(string(Answer),
           '"~w" appears ~D times in this document',
           [Word, Count]).

count_word_in_uri(URI, Word, Count) :-
    html_document(URI, DOM),
    xpath_chk(DOM, //body(text), BodyText),
    tokenize_atom(BodyText, Tokens),
    maplist(downcase_atom, Tokens, LwrTokens),
    aggregate_all(count, member(Word, LwrTokens), Count).

html_document(URL, DOM) :-
    setup_call_cleanup(
        http_open(URL, In, []),
        load_html(In, DOM, []),
        close(In)).

