
# How to install GNU Emacs and this configuration on MS Windows or Linux.
----

## Presentation
* You can read [.emacs.d/features.pdf Slides](https://github.com/claudetete/.emacs.d/blob/master/.emacs.d/features.pdf) created with Emacs which present some features of Emacs, to catch a brief glimpse of Emacs potential
* This GNU Emacs configuration:
  * Uses a system with `profiles`, I use one profile for one computer (work laptop, work desktop, home desktop...)
  * Customizable (by example: you do not want my shortcuts, just one variable set to `nil` in profile)
  * Modes (Add-One/Plugins) are built-in, no need to think about which version is compatible with the configuration
  * Work on both MS Windows and Linux

### You can find here some help to start with Emacs.
* All adding shortcuts are thought for a QWERTY keyboard (some shortcut can be hurtful with an AZERTY keyboard)
* You must have read http://doc.ubuntu-fr.org/emacs#utilisation and [Glossary]
* *C-x* means key 'Control' + key 'x'
* *S-x* means key 'Shift' + key 'x'
* *M-x* means key 'Meta' + key 'x' (Meta = Alt with Emacs)
* *C-x c* means key 'Control' + key 'x' and after key 'c'
* *H-x* means key 'Hyper' + key 'x' (Hyper = Context Menu which is the key just at left of right Control)
* *s-x* means key 'Super' + key 'x' (Super = Windows which is the key Windows between Ctrl and Alt or Command for Apple)

## Installation
See [Wiki Page Installation on MS Windows](https://github.com/claudetete/.emacs.d/wiki/Installation) or  [Wiki Page Installation on Linux](https://github.com/claudetete/.emacs.d/wiki/Installation%20Linux).

## Essential shortcuts
| Shortcut | Description |
| :------: | :---------- |
| C-x C-c | quit Emacs |
| C-x C-s | save a buffer |
| C-x C-f | open a file (when use directory name it switch to Dired mode) |
| C-w | Cut |
| M-w | Copy |
| C-y | Paste |
| C-_ | Undo |
| C-g | Cancel every command |
| C-Space | put a mark (enable selection) |
| M-x myfunction | call myfunction |
| C-s | incremental search |
| F2 | Show/Hide Compile/Occur/Grep/Customize window  (every buffer named *`buffer`*) |
| C-Up/Down | navigate from empty line to empty line |
| C-h f | help about a function |
| C-h k | help about a shortcut |
| C-h a | regex search in help about function |

* To select in a list (like ECB directory) you must middle click to select and go
* The scroll has acceleration by default be careful

You can find more in [.emacs.d/cheatsheet_en.pdf](https://github.com/claudetete/.emacs.d/blob/master/.emacs.d/cheatsheet_en.pdf)

## Misc
See [Wiki Pages](https://github.com/claudetete/.emacs.d/wiki) for more informations
