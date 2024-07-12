---
title: "Undefined Behavior"
description: "It's really not that complicated"
---

There's a lot of confusion among programmers about what C's "undefined
behavior", or "UB" as it is commonly abbreviated, is for, and why C compilers
are allowed to assume UB never happens. This post won't talk about why the spec
contains UB, but will attempt to shed some light on what from my perspective is
a rather confusing aspect of how compilers work with it.

A particularly tricky aspect of UB for the typical C programmer is that it
sometimes causes the compiler to do counterintuitive things, even to seemingly
unrelated parts of the program. It's a confusing moment at first when disabling
optimizations changes the visible effects of your program. If UB represents some
kind of abstract "out-of-spec" error condition, why is the compiler allowed to
change the behavior of statements leading up to it? There is a very reasonable
explanation you could give here, how the spec covers the meaning of whole
programs and not just individual statements, but I think it's much easier
understood via time travel.

The `unreachable()` macro is one way for a program to explicitly invoke
undefined behavior. Because the behavior is undefined, the implementation of
`unreachable()` could be a program that sends a robot back in time to prevent
the code from running, then destroys the entire world so that none of us are
alive in a timeline where `unreachable()` finishes execution. A final
`printf("Undefined behavior can *never* occur.\n");` is the last thing the
universe sees.

This solution may of course have some retroactive effects that at first seem
unrelated. The time traveling robot may choose not to simply terminate the
program right before it enters `unreachable()`, and may instead prevent the
program from being started in the first place. It might even delete the code
that was compiled to produce the program, leaving a cryptic commit message
before deactivating itself in a car crusher. It might go even further back in
time to prevent the programmer's parents from meeting. An oversight in the
robot's programming might even cause it to be overzealous in its interpretation
of its mission, as it points the time machine to "1971, Bell Labs." Technically,
the specification prohibits none of these things.

Pending the development of time travel, though, foresight will have to do.
Timelines containing an invocation of UB can be safely ignored, and while
compilers are not mandated to _prevent_ its occurrence, or even informed that
they should assume it never occurs, it's a useful model for what a conforming
compiler is allowed to do in the remaining timelines. The futures where the
optimizer breaks its neck doing assembly parkour are not futures we expect to
find ourselves in. Or at least, their behavior is undefined.
