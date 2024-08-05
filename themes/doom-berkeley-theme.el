;;; doom-berkeley-theme.el --- inspired by Atom One Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 23, 2016 (28620647f838)
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Maintainer: Henrik Lissner <https://github.com/hlissner>
;; Source: https://github.com/atom/one-dark-ui
;;
;;; Commentary:
;;
;; This themepack's flagship theme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-berkeley-theme nil
  "Options for the `doom-berkeley' theme."
  :group 'doom-themes)

(defcustom doom-berkeley-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-berkeley-theme
  :type 'boolean)

(defcustom doom-berkeley-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-berkeley-theme
  :type 'boolean)

(defcustom doom-berkeley-comment-bg doom-berkeley-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'doom-berkeley-theme
  :type 'boolean)

(defcustom doom-berkeley-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-berkeley-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-berkeley
                "A dark theme inspired by Atom One Dark."

                ;; name        default   256           16
                ((bg         '("#000000" "black"       "black"  ))
                 (fg         '("#d1d6df" "#bfbfbf"     "brightwhite"  ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#000000" "black"       "black"        ))
                 (fg-alt     '("#ffffff" "#2d2d2d"     "white"        ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#424243" "black"       "black"        ))
                 (base1      '("#58595b" "#1e1e1e"     "brightblack"  ))
                 (base2      '("#6d6e70" "#2e2e2e"     "brightblack"  ))
                 (base3      '("#808183" "#262626"     "brightblack"  ))
                 (base4      '("#929496" "#3f3f3f"     "brightblack"  ))
                 (base5      '("#a6a8aa" "#525252"     "brightblack"  ))
                 (base6      '("#bbbcbe" "#6b6b6b"     "brightblack"  ))
                 (base7      '("#d0d1d3" "#979797"     "brightblack"  ))
                 (base8      '("#e6e6e7" "#dfdfdf"     "white"        ))

                 (grey       '("#262424" "#ff6655" "grey"          ))

                 (red        '("#b31d2c" "#ff6655" "red"          ))
                 (orange     '("#ffc200" "#dd8844" "brightred"    ))
                 (green      '("#019a5e" "#99bb66" "green"        ))


                 (cstring    '("#2e8540" "#99bb66" "green"        ))
                 (cclass     '("#ff9d1e" "#99bb66" "green"        ))
                 (cdata      '("#ff9d1e" "#99bb66" "green"        ))
                 (ckey       '("#08a2ff" "#99bb66" "green"        ))
                 (cextra     '("#4c2c92" "#99bb66" "green"        ))
                 ;; (ckey       '("#105bd8" "#99bb66" "green"        ))

                 (teal       '("#019a5e" "#44b9b1" "brightgreen"  ))
                 (yellow     '("#ffc200" "#ECBE7B" "yellow"       ))
                 (blue       '("#08a2ff" "#51afef" "brightblue"   ))
                 (dark-blue  '("#017fcb" "#2257A0" "blue"         ))
                 (magenta    '("#d72f82" "#c678dd" "brightmagenta"))
                 (violet     '("#875afc" "#a9a1e1" "magenta"      ))
                 (cyan       '("#08a2ff" "#46D9FF" "brightcyan"   ))
                 (dark-cyan  '("#08a2ff" "#5699AF" "cyan"         ))

                 ;; These are the "universal syntax classes" that doom-themes establishes.
                 ;; These *must* be included in every doom themes, or your theme will throw an
                 ;; error, as they are used in the base theme defined in doom-themes-base.
                 (highlight      fg-alt)
                 (vertical-bar   (doom-darken base1 0.1))
                 (selection      ckey)
                 (builtin        ckey)
                 (comments       base0)
                 (doc-comments   base0)
                 (constants      fg)
                 (functions      fg)
                 (keywords       ckey)
                 (methods        fg-alt)
                 (operators      red)
                 (type           cclass)
                 (strings        cstring)
                 (variables      fg)
                 (numbers        cdata)
                 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
                 (error          red)
                 (warning        orange)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; These are extra color variables used only in this theme; i.e. they aren't
                 ;; mandatory for derived themes.
                 (modeline-fg              fg-alt)
                 (modeline-fg-alt          base5)
                 (modeline-bg              (if doom-berkeley-brighter-modeline
                                               (doom-darken blue 0.45)
                                             (doom-darken bg-alt 0.1)))
                 (modeline-bg-alt          (if doom-berkeley-brighter-modeline
                                               (doom-darken blue 0.475)
                                             `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
                 (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
                 (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

                 (-modeline-pad
                  (when doom-berkeley-padded-modeline
                    (if (integerp doom-berkeley-padded-modeline) doom-berkeley-padded-modeline 4))))


  ;;;; Base theme face overrides
                (((line-number &override) :foreground base0)
                 ((line-number-current-line &override) :foreground fg-alt)
                 ((font-lock-comment-face &override)
                  :background (if doom-berkeley-comment-bg (doom-lighten bg 0.05) 'unspecified))
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if doom-berkeley-brighter-modeline base8 highlight))

                 (font-lock-function-name-face :inherit 'bold :foreground functions)
                 (font-lock-variable-name-face :inherit 'italic :foreground variables)
                 (font-lock-string-face :inherit 'italic :foreground strings)

                 (window-divider-first-pixel :background base3 :foreground base3)
                 ;; (window-divider-last-pixel :background base3 :foreground base3)
                 (window-divider :background bg :foreground bg)
                                        ;((fringe &override) :box '(:line-width 2 :color red))
                                        ; (fringe :box (:line-width (2 . 0) :color red :style nil))

   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; doom-modeline
                 (doom-modeline-bar :background (if doom-berkeley-brighter-modeline modeline-bg highlight))
                 (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
                 (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
                 (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
                 (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
                 (font-latex-math-face :foreground green)
   ;;;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground red)
                 ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
                 (rjsx-tag :foreground red)
                 (rjsx-attr :foreground orange)
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
                ())

;;; doom-berkeley-theme.el ends here
