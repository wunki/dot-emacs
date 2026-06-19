---
title: "Common Lisp in Emacs"
subtitle: "A Source-First SLY Workflow"
author: "Petar's Emacs Config"
date: "2026-05-24"
geometry: margin=0.7in
fontsize: 12pt
---

# What Changed

This document explains the Common Lisp changes made to the Emacs config and the workflow they are meant to support.

The goal is to keep you close to the code: edit in source buffers, compile small changes into the running Lisp image, use the REPL as a nearby tool, and rely on ASDF for system-level load and test operations.

The changes are in:

- `lisp/pet-lisp.el`
- `lisp/pet-navigation.el`

# Source Buffers Are Primary

Common Lisp development in Emacs is strongest when the source buffer is the main place where work happens.

You edit a function, macro, class, or method in a `.lisp` file, then compile that definition directly into the running Lisp image. You usually do not restart the Lisp process for every change.

The most important habit is:

```text
C-c C-c
```

That compiles the current top-level form. In practice, this means the function, macro, method, class, or variable definition around point is updated live.

This is the heart of interactive Common Lisp development.

# Lisp Mode Indentation

This was added:

```elisp
(use-feature lisp-mode
  :custom
  (lisp-indent-function 'common-lisp-indent-function))
```

Emacs has support for several Lisp dialects. This tells Emacs to use Common Lisp indentation rules in `lisp-mode`.

Your existing source-buffer paredit setup remains:

```elisp
(use-package paredit
  :hook (lisp-mode . paredit-mode))
```

So `.lisp` files still get structural editing.

Paredit is not enabled in the SLY REPL, because it interferes with using `RET` to submit forms there.

# SLY Editing Mode

This was added to the `sly` config:

```elisp
:hook (lisp-mode . sly-editing-mode)
```

`sly-editing-mode` is SLY's source-editing minor mode. It gives Common Lisp source buffers the SLY commands and integrations that talk to the running Lisp image.

It does not start SBCL by itself. It only prepares the buffer so SLY commands are available.

Useful commands in a `.lisp` source buffer:

```text
C-c C-z   start SLY, jump to REPL, or jump back to source
C-c C-c   compile current top-level form
C-c C-k   compile and load current file
C-c C-r   compile selected region
M-.       jump to definition
M-,       jump back
C-c C-d d describe symbol
C-c C-d h open HyperSpec documentation
C-c C-m   macroexpand one step
C-c M-m   macroexpand fully
M-n       next compiler note
M-p       previous compiler note
```

# Completion

This was added:

```elisp
(sly-complete-symbol-function 'sly-flex-completions)
```

This asks SLY to use flexible symbol completion. It should make Lisp symbol completion less strict than exact prefix matching.

Your broader completion stack is still:

- Corfu for in-buffer completion UI
- Vertico for minibuffer completion
- Orderless for flexible minibuffer matching
- Marginalia for annotations
- Consult for navigation/search commands

# ASDF Project Commands

You already had ASDF project root detection:

```elisp
pet/project-find-asdf-system
```

That means Emacs can recognize a directory containing `.asd` files as a project, even if the project is not in Git.

I added commands that use the current project root to find `.asd` files and infer the likely ASDF system name.

New bindings:

```text
C-c C-a l   load current ASDF system
C-c C-a t   test current ASDF system
C-c C-a r   force reload current ASDF system
```

They evaluate forms like:

```lisp
(asdf:load-system "my-system")
(asdf:test-system "my-system")
(asdf:load-system "my-system" :force t)
```

If the project has one `.asd` file, that system is selected automatically.

If the project has multiple `.asd` files, Emacs prompts with completion.

If no `.asd` file is found, Emacs asks for a system name.

One important detail: Emacs finding the project root does not automatically make ASDF know about the system inside SBCL. ASDF still needs to be able to find the system. Common ways to make that work are:

- put projects under `~/common-lisp/`
- use Quicklisp local projects
- configure ASDF source registries
- load from a Lisp image that already knows about your project paths

# REPL Toggle

Your custom `C-c C-z` behavior remains:

```text
C-c C-z
```

From a source buffer:

- starts SLY if needed
- otherwise jumps to the SLY REPL

From the SLY REPL:

- jumps back to the remembered source buffer

This supports a source-first workflow. The REPL is nearby, but it does not become the main editor.

# Popper Integration

The following SLY buffers were added to `popper-reference-buffers`:

```elisp
"\\*sly-mrepl"
"\\*sly-db"
"\\*sly-inspector"
"\\*sly-xref"
"\\*sly-traces"
"\\*sly-compilation"
"\\*sly-macroexpansion"
"\\*sly-description"
```

This means SLY support buffers are treated as temporary popups:

- REPL
- debugger
- inspector
- xref results
- trace dialog
- compilation output
- macroexpansion buffers
- description/doc buffers

Your Popper commands:

```text
C-`     toggle popup
M-`     cycle popups
C-M-`   toggle popup type
```

The intent is that SLY can show useful tools without permanently disrupting the source window layout.

# Expected Workflow

## 1. Open a Common Lisp project

Open a `.lisp` file inside a project with an `.asd` file.

Your config can recognize that directory as an Emacs project even when it is not a Git repository.

## 2. Start SLY

Use:

```text
C-c C-z
```

If SLY is not connected, this starts SBCL through SLY.

Once connected, `C-c C-z` moves between the source buffer and the REPL.

## 3. Load the ASDF system

Use:

```text
C-c C-a l
```

This calls:

```lisp
(asdf:load-system "your-system")
```

Use this when beginning a session, after pulling code, after changing dependencies, or after touching enough files that system-level loading is clearer than compiling individual forms.

## 4. Edit one definition

Make a change in the source buffer.

Then use:

```text
C-c C-c
```

This compiles the current top-level form into the running Lisp image.

This should be the most common command in your loop.

## 5. Compile the whole file when needed

Use:

```text
C-c C-k
```

This compiles and loads the current file.

Use it when you changed multiple related definitions, package-level forms, macros used later in the file, or anything where compiling only one definition is not enough.

## 6. Run a quick experiment

Use:

```text
C-c C-z
```

Type a form in the REPL, submit it with `RET`, then return to source with `C-c C-z`.

The REPL is useful for:

- trying small expressions
- inspecting values
- calling functions manually
- reproducing a bug
- checking state in the live image

But most editing should still happen in source files.

## 7. Run tests

Use:

```text
C-c C-a t
```

This calls:

```lisp
(asdf:test-system "your-system")
```

This assumes the system defines an ASDF test operation.

## 8. Navigate with the Lisp image

Use:

```text
M-.   jump to definition
M-,   jump back
```

This is better than plain text search because SLY asks the running Lisp image where things are defined.

It works especially well for loaded dependencies, generic functions, methods, and symbols whose source location is known to the Lisp image.

For text search, you still have:

```text
M-s M-g   consult-ripgrep
M-s M-l   consult-line
M-s M-b   consult-buffer
M-s M-f   consult-find
```

# Compiler Notes

When you compile a definition or file, SLY can annotate warnings, style warnings, and notes in the source buffer.

Use:

```text
M-n   next compiler note
M-p   previous compiler note
```

The intended loop is:

1. Compile.
2. Read notes inline.
3. Jump through them.
4. Fix the source.
5. Compile again.

# Debugger Workflow

When Lisp signals a condition, SLY opens its debugger buffer.

This is normal Common Lisp development, not a failure mode.

The debugger lets you:

- inspect the condition
- inspect stack frames
- choose restarts
- evaluate expressions in a frame
- recompile source while stopped in the debugger
- continue or retry after fixing code

With Popper, the debugger is treated as a popup, so it should not permanently disturb your layout.

# Macroexpansion

For macro-heavy code, use macroexpansion constantly.

Commands:

```text
C-c C-m   macroexpand one step
C-c M-m   macroexpand fully
```

This is useful when working with:

- custom macros
- DSLs
- iteration libraries
- object systems
- test frameworks
- web or routing libraries

Macroexpansion keeps the source-level abstraction honest by showing the code Lisp will actually compile.

# High-Value Muscle Memory

These are the commands to internalize first:

```text
C-c C-z     REPL/source toggle
C-c C-c     compile current definition
C-c C-k     compile and load current file
C-c C-a l   load ASDF system
C-c C-a t   test ASDF system
M-.         jump to definition
M-,         jump back
M-n         next compiler note
M-p         previous compiler note
C-c C-d d   describe symbol
C-c C-d h   HyperSpec lookup
```

# Practical Rule of Thumb

Use the smallest operation that updates the live image correctly:

```text
Changed one function?       C-c C-c
Changed several forms?      C-c C-k
Changed system structure?   C-c C-a l
Want confidence?            C-c C-a t
Need to poke live state?    C-c C-z
Need to understand code?    M-. / describe / macroexpand
```

# The Intended Feel

The setup is designed around this rhythm:

1. Stay in the source file.
2. Change one thing.
3. Compile it into the live image.
4. Read compiler feedback immediately.
5. Test or poke from nearby.
6. Jump through definitions using SLY.
7. Keep the REPL, debugger, inspector, and xref buffers temporary.

This is the flow Common Lisp is good at: a long-running image, fast incremental compilation, live debugging, and editor commands that understand the running program.
