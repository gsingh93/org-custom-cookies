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

Running `M-x org-custom-cookies-containing-subtree` with `point` anywhere inside the subtree containing the custom cookies will update the headline containing the cookies to this:
```org
* Agenda for today [S: 1:00] [C: 1:20] [E: 1:30]
```

If any changes are made to the scheduled times, clocked times, or effort, running this command again will update the cookie values to the new values.

There are five different functions that can be used for updating custom cookies:

- `org-custom-cookies-update-current-heading`: Update any custom cookies defined in the heading of item point is currently at
- `org-custom-cookies-update-nearest-heading`: Update any custom cookies defined in the heading of the current item if any exist, otherwise search for the closest parent heading with the custom cookie and update that heading. Any further parent headings are not updated.
- `org-custom-cookies-update-subtree`: Updates any custom cookies found in the current heading and any child heading of the current heading
- `org-custom-cookies-update-containing-subtree`: First finds the topmost parent heading that contains a custom cookie, and then updates all custom cookies in headings in that subtree
- `org-custom-cookies-update-all`: Updates all custom cookies in the buffer

TODO: Give examples of each command above?
TODO: Mention how to add this command to the org mode-map
TODO: Mention how to add triggers on effort property change, schedule, and clock out

### Adding Custom Cookies

You can add your own custom cookies by customizing the `org-custom-cookies-alist` variable. The default value is:

```elisp
(("\\[S: ?\\(?:[0-9]*:[0-9]*\\)?\\]" . org-custom-cookies--subtree-scheduled-duration-cookie)
    ("\\[C: ?\\(?:[0-9]*:[0-9]*\\)?\\]" . org-custom-cookies--subtree-clocksum-cookie)
    ("\\[E: ?\\(?:[0-9]*:[0-9]*\\)?\\]" . org-custom-cookies--subtree-effort-cookie))
```

The keys for the `alist` are the regexes to match in a heading. When a regex matches, the corresponding function is called at the beginning of the matching header. The custom cookie that was matched is then replaced by the return value of this function, which itself should also match the regex so the function can be run in the future to update the cookie value. Avoid adding any regexes that could match multiple custom cookie formats, as that will result in one custom cookie function overwriting the result of another custom cookie function.

- add to list (setf + alist-get or just add-to-list?))
- remove from list (alist-get or assq-delete-all or rassq-delete-all)
- overwrite entire list (setq)
- change regex for one entry (setf + alist-get?)

## Comparison to `org-columns`

`org-columns` is often used to achieve what this package does, but there are multiple issues with it. The first issue is purely aesthetic, where some people may not like how the columns look and may prefer the simpler appearance of a cookie instead. The second issue is a lack of customizability in `org-columns`. While `org-columns` does allow you to show the total clocked time, it doesn't allow you to show the total scheduled time, the total effort, or anything other than the built in columns.

TODO: Confirm that org-columns can't show scheduled time or effort
TODO: Link to manual for org-columns
