# negotiation-game

A work-in-progress prototype of a game engine for grand strategy games (a subgenre of RTS), built with Haskell and stack. Tested to work on several Windows 10 machines, Linux support is possible but not guaranteed.

## Current features

The user can hover around the map with WASD and zoom in or out using the mouse wheel or +/- numpad keys. (At the moment I do not own a mouse; therefore, zoom speed is optimized for the numpad keys.)

The map is divided into clickable provinces which display a draggable GUI window with additional information. All windows can be closed with a button in the corner. 

There is a debug window in the upper left corner which can be brought back if closed by pressing C.

## Dependencies

The project is built using stack. It assumes (in stack.yaml) that at `..\gpipe\gpipe-and-friends` there is a clone of the **https://github.com/asfarious/gpipe-and-friends** repository. GPipe is a functional wrapper around OpenGL using the type system to ensure OpenGL invariants. (Unlike most OpenGL bindings for Haskell, which directly expose the OpenGL state machine methods.)

## Architecture

The entry point is `app/Main.hs`, which handles GPipe machinery and wraps the rest of the code. The raw, impure input handling is done in `src/Input.hs` and then processed by the pure code in `src/ProcessEvents.hs`. The rest of `src/` is self-explanatory. Shaders are written entirely in Haskell, text is displayed by loading a character atlas into a texture and then sampling it.

In `src/GUI/` there is a minimalistic EDSL for describing GUI which greatly reduces the boilerplate. Looking at dumps from the compiler, I believe that it's fully erased at the level of GHC Core and is therefore a zero-cost abstraction.

Code in `src/Board/` renders the map. It is not very interesting because I had little time to work on this part of the project. 