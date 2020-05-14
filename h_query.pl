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

:- module(h_query,
          [ h_user/3,                   % -UserID,-GroupId,+Options
            h_annotation/2              % +AnnotId,-Annot
          ]).
:- use_module(h_api).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(broadcast)).
:- use_module(library(dialect/xsb/increval)).

:- table ( h_user/3,
           h_annotation/2
         ) as (shared, incremental).

/** <module> Access hypothesis objects

This module provides cached access to   objects  in the hypothesis store
such that we can efficiently reason about them.
*/

%!  h_user(-UserID, -GroupId, +Options) is det.
%
%   Get the user and group  id  of   the  current  process. If an option
%   `group` is provided, get the id of this   group.  Else get the id of
%   the public group.

h_user(UserId, GroupId, Options) :-
    user_profile(Profile),
    UserId = Profile.userid,
    option(group(GroupName), Options, 'Public'),
    (   member(Group, Profile.groups),
        atom_string(GroupName, Group.name)
    ->  atom_string(GroupId, Group.id)
    ;   existence_error(hypothesis_group, GroupName)
    ).

%!  h_annotation(+AnnotId, -Annot) is semidet.
%
%   Get the annotation object belonging to an annotation.   Fails
%   silently if the annotation (no longer) exists.

h_annotation(AnnotId, Annot) :-
    E = error(_,_),
    catch(fetch_annotation(AnnotId, Annot),
          E,
          ( print_message(warning, E),
            fail)).


		 /*******************************
		 *        EVENT HANDLING	*
		 *******************************/

:- initialization listen(h(Event), event(Event)).

%!  event(+Event) is nondet.
%
%   Listen to annotation events from  the   real  time listener, notably
%   invalidating objects that we have cached in tables.

event(deleted_annotations(List)) :-
    forall(member(Id, List),
           incr_invalidate_calls(h_annotation(Id, _))).
