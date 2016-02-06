elm-version

this is a port of the purescript code to elm, using elm-html and elm-svg instead of JS to render the app

this was made by hacking the following changes into the Core Keyboard.elm, because keypresses do not emit events for arrow keys.

```diff
--- a/src/Keyboard.elm
+++ b/src/Keyboard.elm
@@ -1,7 +1,7 @@
 module Keyboard
     ( arrows, wasd
     , enter, space, ctrl, shift, alt, meta
-    , isDown, keysDown, presses
+    , isDown, keysDown, presses, downs
     ) where

 {-| Library for working with keyboard input.
@@ -201,3 +201,7 @@ presses : Signal KeyCode
 presses =
   Signal.map .keyCode Native.Keyboard.presses

+{-| A Signal of keyCodes from the window 'keydown' event -}
+downs : Signal KeyCode
+downs =
+  Signal.map .keyCode Native.Keyboard.downs
```
