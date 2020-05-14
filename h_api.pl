:- module(h_api,
          [ api/1,                              % -Response
            create_annotation/2,                % +RequestBody, -Response
            delete_annotation/2,                % +Id, -Response
            fetch_annotation/2,                 % +Id, -Response
            update_annotation/2,                % +Id, -Response
            flag_annotation/2,                  % +Id, +RequestBody
            unhide_annotation/1,                % +Id
            hide_annotation/2,                  % +Id, +RequestBody
            fetch_group_list/2,                 % -Response, +Options
            create_group/2,                     % +RequestBody, -Response
            fetch_group/3,                      % +Id, -Response, +Options
            update_group/2,                     % +Id, -Response
            upsert_group/3,                     % +Id, +RequestBody, -Response
            group_members/2,                    % +Id, -Response
            delete_group_member/2,              % +Id, +User
            add_group_member/3,                 % +Id, +User, +RequestBody
            user_profile/1,                     % -Response
            user_groups/1,                      % -Response
            search_annotations/2,               % -Response, +Options
            create_user/2,                      % +RequestBody, -Response
            update_user/2,                      % +Username, -Response
            user_info/2                         % +User, -Response
          ]).
:- use_module(library(openapi)).
:- use_module(library(option)).

:- openapi_client('hypothesis-v1.yaml', [warn(false)]).

%! api(-Response) is det.
%
%  Service root
%  Provides a list of links to resources offered by the API.
%
%  @arg Response -
%       Success
%
%  @see Path = /

%! create_annotation(+RequestBody, -Response) is det.
%
%  Create a new annotation
%
%  Authentication options:
%   - ApiKey
%
%  @arg RequestBody -
%       Full representation of Annotation resource and applicable relationships.
%       _Note_: While the API accepts arbitrary Annotation selectors in the
%       `target.selector` property, the Hypothesis client currently supports
%       `TextQuoteSelector`, `RangeSelector` and `TextPositionSelector` selector.
%  @arg Response object([p(created,date_time,true),p(flagged,boolean,true),p(group,string,true),p(hidden,boolean,true),p(id,string,true),p(links,object,true),p(permissions,object,true),p(references,array(string),false),p(tags,array(string),true),p(target,array(object),true),p(text,string,true),p(updated,date_time,true),p(uri,uri,true),p(user,string,true),p(user_info,object,false)])
%       Success
%
%  @see Path = /annotations

%! delete_annotation(+Id, -Response) is det.
%
%  Delete an Annotation
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id string
%  @arg Response object([p(deleted,boolean,true),p(id,string,true)])
%       Success
%
%  @see Path = /annotations/{id}

%! fetch_annotation(+Id, -Response) is det.
%
%  Fetch an Annotation
%
%  Authentication options:
%   - ApiKey
%   - no authentication required
%
%  @arg Id string
%  @arg Response object([p(created,date_time,true),p(flagged,boolean,true),p(group,string,true),p(hidden,boolean,true),p(id,string,true),p(links,object,true),p(permissions,object,true),p(references,array(string),false),p(tags,array(string),true),p(target,array(object),true),p(text,string,true),p(updated,date_time,true),p(uri,uri,true),p(user,string,true),p(user_info,object,false)])
%       Success
%
%  @see Path = /annotations/{id}

%! update_annotation(+Id, -Response) is det.
%
%  Update an Annotation
%  This endpoint is available under both the `PATCH` and `PUT`
%  request methods. Both endpoints have PATCH-characteristics
%  as defined in [RFC5789](https://tools.ietf.org/html/rfc5789#section-1),
%  meaning the request body does not have to include the whole annotation
%  object.
%  
%  New implementations should use the `PATCH` request method, and existing
%  implementations continue to work under `PUT` but should switch to `PATCH`.
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id string
%  @arg Response object([p(created,date_time,true),p(flagged,boolean,true),p(group,string,true),p(hidden,boolean,true),p(id,string,true),p(links,object,true),p(permissions,object,true),p(references,array(string),false),p(tags,array(string),true),p(target,array(object),true),p(text,string,true),p(updated,date_time,true),p(uri,uri,true),p(user,string,true),p(user_info,object,false)])
%       Success
%
%  @see Path = /annotations/{id}

%! flag_annotation(+Id, +RequestBody) is det.
%
%  Flag an annotation
%  Flag an annotation for review (moderation). The moderator of the group
%  containing the annotation will be notified of the flag and can decide
%  whether or not to hide the annotation. Note that flags persist and
%  cannot be removed once they are set.
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id string
%  @arg RequestBody -
%
%  @see Path = /annotations/{id}/flag

%! unhide_annotation(+Id) is det.
%
%  Show an annotation
%  Show/"un-hide" an annotation. The authenticated user needs to have the
%  moderate permission for the group that contains the annotation—this
%  permission is granted to the user who created the group.
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id string
%
%  @see Path = /annotations/{id}/hide

%! hide_annotation(+Id, +RequestBody) is det.
%
%  Hide an annotation
%  Hide an annotation. The authenticated user needs to have the moderate
%  permission for the group that contains the annotation—this permission
%  is granted to the user who created the group.
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id string
%  @arg RequestBody -
%
%  @see Path = /annotations/{id}/hide

%! fetch_group_list(-Response, +Options) is det.
%
%  Get a list of Groups
%  Retrieve a list of applicable Groups, filtered by authority and target
%  document (`document_uri`). Also retrieve user's private Groups.
%
%  Authentication options:
%   - ApiKey
%   - no authentication required
%
%  @arg Response -
%       Success
%  @arg Options 
%       - authority(+string)
%         Filter returned groups to this authority. For authenticated requests,
%         the user's associated authority will supersede any provided value.
%       - document_uri(+uri)
%         Only retrieve public (i.e. non-private) groups that apply to a
%         given document URI (i.e. the target document being annotated).
%       - expand(+array(enum([organization,scopes])))
%         One or more relations to expand for a group resource
%
%  @see Path = /groups

%! create_group(+RequestBody, -Response) is det.
%
%  Create a new group
%  Create a new, private group for the currently-authenticated user.
%
%  Authentication options:
%   - AuthClientForwardedUser
%   - ApiKey
%
%  @arg RequestBody -
%       Full representation of Group resource
%  @arg Response object([p(groupid,string,true),p(id,string,true),p(links,object,true),p(name,string,true),p(organization,oneOf([string,object([p(default,boolean,true),p(id,string,true),p(logo,uri,true),p(name,string,true)])]),true),p(public,boolean,true),p(scoped,boolean,true),p(scopes,object([p(enforced,boolean,true),p(uri_patterns,array(string),true)]),false),p(type,enum([private,open,restricted]),true)])
%       Success
%
%  @see Path = /groups

%! fetch_group(+Id, -Response, +Options) is det.
%
%  Fetch a Group
%  Fetch a single Group resource.
%
%  Authentication options:
%   - AuthClient
%   - ApiKey
%   - no authentication required
%
%  @arg Id oneOf([string,string])
%  @arg Response -
%       Success
%  @arg Options 
%       - expand(+array(enum([organization,scopes])))
%         One or more relations to expand for a group resource
%
%  @see Path = /groups/{id}

%! update_group(+Id, -Response) is det.
%
%  Update a Group
%  Update a Group resource.
%
%  Authentication options:
%   - AuthClient
%   - AuthClientForwardedUser
%   - ApiKey
%
%  @arg Id oneOf([string,string])
%  @arg Response object([p(groupid,string,true),p(id,string,true),p(links,object,true),p(name,string,true),p(organization,oneOf([string,object([p(default,boolean,true),p(id,string,true),p(logo,uri,true),p(name,string,true)])]),true),p(public,boolean,true),p(scoped,boolean,true),p(scopes,object([p(enforced,boolean,true),p(uri_patterns,array(string),true)]),false),p(type,enum([private,open,restricted]),true)])
%       Success
%
%  @see Path = /groups/{id}

%! upsert_group(+Id, +RequestBody, -Response) is det.
%
%  Create or Update a Group
%  Update the group with the indicated `id` or create one if it does
%  not exist.
%
%  Authentication options:
%   - ApiKey
%   - AuthClientForwardedUser
%
%  @arg Id oneOf([string,string])
%  @arg RequestBody -
%       Full representation of Group resource
%  @arg Response object([p(groupid,string,true),p(id,string,true),p(links,object,true),p(name,string,true),p(organization,oneOf([string,object([p(default,boolean,true),p(id,string,true),p(logo,uri,true),p(name,string,true)])]),true),p(public,boolean,true),p(scoped,boolean,true),p(scopes,object([p(enforced,boolean,true),p(uri_patterns,array(string),true)]),false),p(type,enum([private,open,restricted]),true)])
%       Success
%
%  @see Path = /groups/{id}

%! group_members(+Id, -Response) is det.
%
%  Get group members
%  Fetch a list of all members (users) in a group. Returned user resource only
%  contains public-facing user data. Authenticated user must have read access
%  to the group. Does not require authentication for reading members of
%  public groups. Returned members are unsorted.
%
%  Authentication options:
%   - AuthClient
%   - ApiKey
%   - no authentication required
%
%  @arg Id oneOf([string,string])
%  @arg Response -
%       Success
%
%  @see Path = /groups/{id}/members

%! delete_group_member(+Id, +User) is det.
%
%  Remove member from group
%  Remove a user from a group. At present, this endpoint only allows
%  the removal as one's self (authenticated with API Key) from the
%  indicated group.
%
%  Authentication options:
%   - ApiKey
%
%  @arg Id oneOf([string,string])
%  @arg User enum([me])
%       Currently, only the literal value `me` is accepted
%
%  @see Path = /groups/{id}/members/{user}

%! add_group_member(+Id, +User, +RequestBody) is det.
%
%  Add member to group
%  Add a user as a member to a group. This endpoint is only accessible to
%  requests authenticated with `AuthClient` credentials and is restricted
%  to users and groups within the associated authority.
%
%  Authentication options:
%   - AuthClient
%
%  @arg Id oneOf([string,string])
%  @arg User string
%       Unique identifier for a user, with specified authority.
%       The userID should be of the format `acct:<username>@<authority>`
%  @arg RequestBody -
%
%  @see Path = /groups/{id}/members/{user}

%! user_profile(-Response) is det.
%
%  Fetch user's profile
%  Fetch profile information for the currently-authenticated user.
%
%  Authentication options:
%   - ApiKey
%   - no authentication required
%
%  @arg Response -
%       Success
%
%  @see Path = /profile

%! user_groups(-Response) is det.
%
%  Fetch user's groups
%  Fetch the groups for which the currently-authenticated user is a member.
%
%  Authentication options:
%   - ApiKey
%   - no authentication required
%
%  @arg Response -
%       Success
%
%  @see Path = /profile/groups

%! search_annotations(-Response, +Options) is det.
%
%  Search for annotations
%
%  Authentication options:
%   - ApiKey
%   - no authentication required
%
%  @arg Response object([p(rows,array(object([p(created,date_time,true),p(flagged,boolean,true),p(group,string,true),p(hidden,boolean,true),p(id,string,true),p(links,object,true),p(permissions,object,true),p(references,array(string),false),p(tags,array(string),true),p(target,array(object),true),p(text,string,true),p(updated,date_time,true),p(uri,uri,true),p(user,string,true),p(user_info,object,false)])),true),p(total,integer,true)])
%       Success
%  @arg Options 
%       - limit(+numeric(integer,between(0,200)))
%         The maximum number of annotations to return.
%       - sort(+enum([created,updated,group,id,user]))
%       - search_after(+string)
%         Define a start point for a subset (page) of annotation search results.
%         
%         Working against the sorted, full set of annotation records matching the current
%         search query, the service will examine the values present in the field by which
%         annotations are sorted (i.e. `sort`). The returned subset
%         of search results will begin with the first annotation whose `sort` field's value
%         comes after the value of `search_after` sequentially.
%         
%         The format of this property depends on the current value of `sort`.
%         When `search_after` is used in conjunction with a chronological `sort` value—e.g.
%         `updated`, `created`—this parameter should be formatted as an ISO 8601 string. It
%         may also be formatted in ms (milliseconds) since the Epoch.
%         
%         *Expanded example*
%         
%         Given a query containing a (URI-encoded) `search_after` value of `2019-01-03T19:46:09.334539+00:00`:
%         
%         >`sort=created&search_after=2019-01-03T19%3A46%3A09.334539%2B00%3A00`
%         
%         The returned results would begin with the record immediately subsequent to the annotation
%         created at `2019-01-03T19:46:09.334539+00:00` in the full set of results. If there is no
%         annotation in the full result set whose `created` value exactly matches
%         `2019-01-03T19:46:09.334539+00:00`,
%         
%         the returned subset will begin with the first annotation whose `created` value
%         comes sequentially "after" `2019-01-03T19:46:09.334539+00:00` in the full, sorted set.
%         
%         _Note:_ `search_after` provides an efficient, stateless paging mechanism. Its
%         use is preferred over that of `offset`.
%       - offset(+numeric(integer,max(9800)))
%         The number of initial annotations to skip in the result set.
%         
%         May be used for pagination of result sets. The usage of `search_after` is preferred,
%         especially for large batches, as it is considerably more efficient.
%       - order(+enum([asc,desc]))
%         The order in which the results should be sorted.
%       - uri(+uri)
%         Limit the results to annotations matching the specific URI or equivalent URIs.
%         
%         URI can be a URL (a web page address) or a URN representing another kind of
%         resource such as DOI (Digital Object Identifier) or a PDF fingerprint.
%         
%         Examples:
%         
%         * `http://example.com/articles/01/name` (URL)
%         * `doi:10.1.1/1234` (DOI)
%         * `urn:x-pdf:1234` (PDF fingerprint)
%       - url(+uri)
%         Alias of `uri`
%       - 'uri.parts'(+string)
%         Limit the results to annotations containing the given keyword (tokenized chunk) in
%         the URI. The value must exactly match an individual URI keyword.
%         
%         URIs are split on characters `#+/:=?.-` into their keywords.
%         
%         *Expanded example*
%         
%         Given a value of `yogur`, annotations with any of the following URIs would match:
%         
%         * `https://www.yogur.com/foo/bar`
%         * `https://www.example.com/yogur/eatmore.html`
%         * `https://www.example.com/foo/eat-more-yogur-this-year`
%         
%         The following would not be matches:
%         
%         * `https://www.yogurt.com/foo/bar`
%         * `https://www.example.com/yogurt/eatmore.html`
%         * `https://www.example.com/foo/eat-more-yogurt-this-year`
%       - wildcard_uri(+string)
%         Limit the results to annotations whose URIs match the wildcard pattern.
%         
%         `*` will match any character sequence (including an empty one),
%         and a `_` will match any single character. Wildcards are only permitted
%         within the path and query parts of the URI. Escaping wildcards is not supported.
%         
%         Examples of valid values:
%         
%         * `http://foo.com/*`
%         * `urn:x-pdf:*`
%         * `file://localhost/_bc.pdf`
%         
%         Examples of invalid values (not within path or query parts of URI):
%         
%         * `*foo.com`
%         * `u_n:*`
%         * `file://*`
%         * `http://foo.com*`
%         
%         <mark>This feature is experimental and the API may change.</mark>
%       - user(+string)
%         Limit the results to annotations made by the specified user.
%       - group(+string)
%         Limit the results to annotations made in the specified group (by group ID).
%       - tag(+string)
%         Limit the results to annotations tagged with the specified value.
%         
%         For example: `artificial intelligence` will find all annotations whose tags
%         contain both `artificial` **AND** `intelligence`.
%       - tags(+array(string))
%         Similar to `tag` but allows a comma-separated list of multiple tags.
%         
%         For example: `[intelligence,artificial]` will find all annotations whose tags
%         contain both `artificial` **AND** `intelligence`.
%       - any(+string)
%         Limit the results to annotations who contain the indicated keyword
%         in any of the following fields:
%         
%         * `quote`
%         * `tags`
%         * `text`
%         * `url`
%       - quote(+string)
%         Limit the results to annotations that contain this text inside
%         the text that was annotated.
%         
%         For example: `unicorn helmets` would return all annotations containing
%         `unicorn` **OR** `helmets` in their quoted (i.e. annotated) text.
%       - references(+string)
%         Returns annotations that are replies to this parent annotation ID.
%       - text(+string)
%         Limit the results to annotations that contain this text in their
%         textual body.
%         
%         For example: `penguin strength` would return all annotations containing
%         `penguin` **OR** `strength` in their text (body) content.
%
%  @see Path = /search

%! create_user(+RequestBody, -Response) is det.
%
%  Create a new user
%  Create a new user within the client's associated authority.
%
%  Authentication options:
%   - AuthClient
%
%  @arg RequestBody -
%       Full representation of User resource
%  @arg Response allOf([object,object([p(userid,string,true)])])
%       Success
%
%  @see Path = /users

%! update_user(+Username, -Response) is det.
%
%  Update a user
%  Update an existing user.
%
%  Authentication options:
%   - AuthClient
%
%  @arg Username string
%       The user's username
%  @arg Response allOf([object,object([p(userid,string,true)])])
%       Success
%
%  @see Path = /users/{username}

%! user_info(+User, -Response) is det.
%
%  Fetch a user
%  Fetch a single user. This endpoint is only accessible to
%  requests authenticated with `AuthClient` credentials and is restricted
%  to users within the associated authority.
%
%  Authentication options:
%   - AuthClient
%
%  @arg User string
%       Unique identifier for a user, with specified authority.
%       The userID should be of the format `acct:<username>@<authority>`
%  @arg Response allOf([object,object([p(userid,string,true)])])
%       Success
%
%  @see Path = /users/{user}

