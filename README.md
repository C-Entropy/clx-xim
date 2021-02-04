Head: master Update -clx-xim-get-servers- Push: git@github.com:C-Entropy/clx-xim.git remote does not exist

Untracked files (3) .gitignore README.markdown tests/test-atom.lisp

Unstaged changes (1) modified README.org @@ -4,28 +4,3 @@ for now my goal is to port [xcb-imdkit](https://github.com/fcitx/xcb-imdkit/) to clx **\*** further make it more 'lisper' -\* take a look at demo -\*\* TODO compound text init

-   xcb<sub>compound</sub><sub>text</sub><sub>init</sub>()
-   I guess this has something to done with utf-8, not clearly known what this suppose to do yet.

-\*\* DONE open connection to X/ open a new window for test

-   just normal code

-\*\* TODO open connection to XIM

-   xcb<sub>xim</sub><sub>open</sub>

-**\* TODO process event -\*\*** TODO if has event -**\*\*** TODO if event not filter -****\***** TODO if ic exist && event is key<sub>press</sub> or key<sub>release</sub> -****\***** TODO xcb<sub>xim</sub><sub>forward</sub><sub>event</sub> -****\***** TODO else free event -**\*** TODO if no event

-   close xcb<sub>xim</sub>&window
-

-

-\* xcb<sub>xim</sub><sub>set</sub><sub>im</sub><sub>callback</sub> -\* xcb<sub>xim</sub><sub>set</sub><sub>log</sub><sub>handler</sub> -\* xcb<sub>xim</sub><sub>open</sub> -\* xcb<sub>xim</sub><sub>filter</sub><sub>event</sub> -\* xcb<sub>xim</sub><sub>forward</sub><sub>event</sub> -\* xcb<sub>xim</sub><sub>close</sub> -\* xcb<sub>xim</sub><sub>destroy</sub>

Recent commits c9fcfc4 master Update -clx-xim-get-servers- 66ae708 Update -clx-xim-get-servers- 3a8230c Update -clx-xim-init- 8790b30 Update -clx-im-init-atoms- 4c935b3 Add -clx-xim-get-servers- 2fa4143 Add fun: -clx-change-event-mask- a22b84a Put defclass-easy into utils. 1adb6d3 Restart 96d1fd1 Update README.org 6b49179 First commit, almost done demo and xcl-im-create, next goal i xcb<sub>xim</sub><sub>filter</sub><sub>destroy</sub><sub>window</sub>