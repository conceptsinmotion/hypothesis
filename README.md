# A SWI-Prolog interface to Hypothesis

[Hypothesis](https://web.hypothes.is/) is a platform   for making shared
annotation about any document on the web.  This repo contains a proof of
concept for interacting with the API of hypothesis.

As is, it provides a bot that implements the following scenario:

  - Create two users on Hypothesis that share a group. Say `user`, `bot`
    and `group`.
  - Get the API key for `bot`.
  - run ``swipl test.pl`` and enter the above API key and the group
    name `group`.
  - Install the Chrome pluggin, go to some web page and login as
    `user`.
  - Make an annotation on the page and enter the text (replace
    _someword_ with any word you want to see counted.

	How often is "someword" used?

  - If all goes well, the bot replies to your annotation.  This is
    made visible using a small red icon in the sidebar.  Download
    the new annotations.

## Installation

  - Install the most recent development version for SWI-Prolog
    (8.1.31 or later)
  - Install the openapi pack using the command above.  This should
    install version 0.2 or later of this pack.

	?- pack_install(openapi).

### Further reading

  - https://h.readthedocs.io/en/latest/api/
  - https://h.readthedocs.io/en/latest/api-reference/hypothesis-v1.yaml
