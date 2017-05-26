# Emacs Taquouner, dotemacs with profiles
----

## Presentation
Taquouner, a dotemacs configuration with porfile, is almost daily used since 2006, started with one file of hundred of copied/pasted elisp line.
Then started to grow faster few years later to manage multiple computer and environment (linux/windows, home/work, etc).
It follow my needs of embedded developer, a lot of C and some scripting langages.

* It contains:
  * A system with `profiles`, I use one profile for one computer/system (work laptop, work desktop, home desktop...)
    * Customizable (by example: you do not want my shortcuts, just one variable set to `nil` instead of `t` in profile)
    * Options to gently start with emacs (shift selection, universal Cut/Copy/Paste shortcut, No backup file, usual scroll behavior, etc)
  * Modes (Add-On/Plugins) are built-in, no need to think about which version is compatible with the configuration or configure it
    * Helm (replace all dialog box for anything)
    * Projectile (project manager)
    * GNU Global (symbol/tag manager)
    * Company (completion)
    * Browse Kill Ring (visualize clipboard history)
    * Undo Tree (visualize undo stack)
    * Powerline (modern status bar)
    * Fold Dwim (fold/unfold (hide/show) block of source code)
    * Ack/Ag/Pt (faster grep replacement)
    * AceJump/Avy (faster movement in displayed text)
    * AUCTeX (LaTeX mode)
    * Rainbow (display RGB hexa value in corresponding color)
  * Languages
    * C
    * Python
    * Batch
    * AutoHotkey
    * RtRt
  * Version Control / Configuration Management
    * Git
    * Svn
    * ClearCase
    * Synergy
  * Interface
    * simple font settings
    * include theme
    * color match parentheses
    * highlight current line
    * simple window title settings
    * start in fullscreen
  * Environment
    * Work on both MS Windows and Linux
    * Should work with Emacs 23.3 to 25.1
    * Daily used with Emacs 25.1 on Windows 7 and on ArchLinux

I deliberately try to take point of view of people not used to Emacs.
* ~~You can read [.emacs.d/features.pdf Slides](https://github.com/claudetete/.emacs.d/blob/master/.emacs.d/features.pdf) created with Emacs which present some features of Emacs, to catch a brief glimpse of Emacs potential~~ Deprecated, must be updated.

### You can find here some help to start with Emacs principle.
* All adding shortcuts are thought for a QWERTY keyboard (some shortcut can be hurtful with an AZERTY keyboard)
* You must have read http://doc.ubuntu-fr.org/emacs#utilisation and [Glossary]
* *C-x* means key 'Control' + key 'x'
* *S-x* means key 'Shift' + key 'x'
* *M-x* means key 'Meta' + key 'x' (Meta = Alt with Emacs)
* *C-x c* means key 'Control' + key 'x' and after key 'c'
* *H-x* means key 'Hyper' + key 'x' (Hyper = Context Menu which is the key just at left of right Control)
* *s-x* means key 'Super' + key 'x' (Super = Windows which is the key Windows between Ctrl and Alt or Command for Apple)

## Installation
See [Installation on MS Windows](https://github.com/claudetete/.emacs.d/wiki/Installation) or [Installation on Linux](https://github.com/claudetete/.emacs.d/wiki/Installation%20Linux) on wiki of this repository.

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
| F2 | Show/Hide Compile/Occur/Grep/Customize window (from my configuration) |
| C-Up/Down | navigate from empty line to empty line |
| C-h f | help about a function |
| C-h k | help about a shortcut |
| C-h a | regex search in help about function |


* The mouse scroll has acceleration by default be careful (I rarely use it)

You can find more in [.emacs.d/cheatsheet_en.pdf](https://github.com/claudetete/.emacs.d/blob/master/.emacs.d/cheatsheet_en.pdf)

## Issues/Update
If you want update some mode, fix or modify this configuration, fork it and ask for a pull request.

## Taquouner ethymology
Taquouner is a french dialiect verb no more used which means "work but with few results"
