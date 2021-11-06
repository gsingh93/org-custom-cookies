[![MELPA](https://melpa.org/packages/org-custom-cookies-badge.svg)](https://melpa.org/#/org-custom-cookies)

# org-custom-cookies

A package that allows you to define custom `org-mode` statistics cookies.

## Installation

`org-custom-cookies` can be installed from [MELPA](https://melpa.org/) with the following command:
```
M-x package-install RET org-custom-cookies RET
```

After installing add `(require 'org-custom-cookies)` to your Emacs init file, or if you're using [`use-package`](https://github.com/jwiegley/use-package), add the following:
```elisp
(use-package org-custom-cookies)
```

## Usage

`org-custom-cookies` allows you to define your own custom cookies, but it comes with three cookie types built-in. The `[S:]` cookie will be replaced with the sum of all the scheduled time in the subtree, `[C:]` will be replaced with the total clocked time (equivalent to the `CLOCKSUM` special property), and `[E:]` will be replaced with the sum of all the `Effort` properties in the subtree. For example:
```org
* Agenda for today [S:] [C:] [E:]
** Team meeting
SCHEDULED: <2021-11-05 Fri 10:00-11:00>
** Work on project 1
:PROPERTIES:
:Effort:   1:00
:END:
:LOGBOOK:
CLOCK: [2021-11-05 Fri 11:07]--[2021-11-05 Fri 11:32] =>  0:25
CLOCK: [2021-11-05 Fri 11:40]--[2021-11-05 Fri 12:10] =>  0:30
:END:
** Work on project 2
*** Subtask 1
:PROPERTIES:
:Effort:   0:30
:END:
*** Subtask 2
:LOGBOOK:
CLOCK: [2021-11-05 Fri 12:20]--[2021-11-05 Fri 12:45] =>  0:25
:END:
```

Running `M-x org-custom-cookies-update` with `point` anywhere inside the subtree containing the custom cookies will update the headline containing the cookies to this:
```org
* Agenda for today [S: 1:00] [C: 1:20] [E: 1:30]
```

If any changes are made to the scheduled times, clocked times, or effort, running this command again will update the cookie values to the new values.

TODO: Mention how to add this command to the org mode-map

### Adding Custom Cookies

Note that the cookie regexes can't match each other

`org-custom-cookies-alist`
- add to list
- remove from list
- overwrite entire list
- change regex for one entry

TODO: Make a note about org-column-mode
