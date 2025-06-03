---
title: "This Is Not Really About AI"
description: "The world doesn't need another AI take, but don't worry: this isn't that"
banner: "2025-06-02-this-is-not-really-about-ai.jpg"
banner-width: "1400"
banner-height: "700"
---

I'm not going to talk about whether AI is good or bad, or whether it belongs in
software. I'm not even sure how I feel about it. I'd say cautiously optimistic,
maybe, pending more contextual concerns. But I think the conversation has led to
AI advocacy being too eager, perhaps gleefully so, to be done with writing code
by hand. In my opinion, writing code is here to stay.

As I understand it, the vision goes something like this: by having the computer
write the code, the programmer only needs to spend time verifying the code's
correctness before moving on, greatly reducing the time spent programming. To
their credit, many AI advocates are quick to point out the importance of the
verification step, likening the use of AI to how senior engineers guide and
supervise junior engineers.

That's fine. Desirable, even. And not unfamiliar to anyone who, like myself, has
worked in a senior software engineering role. From this perspective it's easy to
shrug off AI skeptics as overly sentimental and stuck in the past, clinging to
an increasingly irrelevant era of expensive artisanship. We should be _glad_
that programming can be done mostly by simply verifying code that was cheap and
quick to produce, since it's much faster to read code than to write it.

And there's truth to that perspective. When writing new code, a lot of time is
spent transcribing ideas into code that's easily verified, and comparatively
little time spent reading it. This is, to a degree, a consequence of valuing
"readable" code: your code _should_ be quick to read. You _should_ be suspicious
of anything that takes a lot of thought or that might mislead the person who
reads it. The result is that we spend a lot of time writing stuff that simply
looks correct at a glance, sequestering the tricky parts in clearly-marked
places with easy-to-use APIs. Or we try to, anyway.

It's nice to imagine that I could press a button to generate some code, check
that it's what I wanted, and move on. This workflow takes advantage of my
ability to read code critically, something I'd have to do anyway when writing
it. In fact I'd say reading code is my most valuable programming skill, since in
the highly collaborative world of software engineering you are constantly
encountering code you didn't write. Even your own code can seem unfamiliar mere
weeks after writing it. Fixing bugs, a frequent task in almost every codebase
(sometimes more frequent than adding new code) requires carefully reading code
that at one point seemed correct. Occasionally days of studying a codebase
culminate in a one line bug fix. When you're good at reading code critically,
it's easy to see the appeal of a workflow for writing new code that consists
almost exclusively of reading it.

But my ability to read code critically was developed over years of _writing_
code. My ability to judge the correct use of an API or the right iteration
strategies or even just the right CSS properties comes from lots of experience
using them directly. The edge cases and caveats of a particularly tricky
algorithmic task, a close call with an off-by-one error, and so on: these things
usually only become apparent to me after having grappled with something similar
myself. I've written and then fixed broken DFS implementations more times than I
can count. Through experience I've built up a huge library of familiar problems
and common mistakes, a library I draw upon constantly when reading code, and
_especially_ when reading code critically.

To read code effectively, especially for the purpose of validating its
correctness, it needs to be something you could have written yourself. If you
can't have written it yourself, you either need to learn enough about the
problem until you can, or simply trust that the author got it correct. This has
always been true, but it's even more relevant now that computers can quickly and
cheaply generate large amounts of functioning code. Somebody has to read all of
that.

So to what degree are we supposed to trust that AI-generated code is correct? AI
advocates seem to differ on this. Some say we shouldn't trust the AI at all, and
that it's irresponsible not to verify everything they produce. At the other end
of the spectrum, people say we can trust the AI to do everything, even generate
its own tests and fix its own bugs. In the world where I'm an AI advocate, I'm
in the former camp. I don't even trust code I've written myself, let alone code
that came from somewhere else.

To those AI advocates who afford any amount of blind trust to AI-generated code:
sorry, I simply disagree. For anything even moderately important, it'll take a
lot for me to be comfortable without a human in the loop. Maybe you think I
value the cost of mistakes too highly. That might be true, but that's a topic
for some other time.

However, to those who say we should default to being skeptical of everything
written by an AI: don't overlook the value of programming by hand. Take some
time every once in a while to write something yourself, especially if there's
some novelty involved. Find some joy in the craft of carefully tackling a tricky
problem. Make it readable, too. Writing code is how you get better at reading
code, and reading code is your most valuable skill, now more than ever.
