---
title: "Reader Mode: It Just Works"
---

I'm the kind of internet user for whom the [Firefox Reader View
feature][mozilla] is only occasionally useful. Most mainstream browsers have
something like it these days. I'm sure there are people out there who can't
imagine using the internet without it. For me, it's a lot like having a
unique screwdriver that's a better choice for some jobs than resorting to a
suitably-sized flathead and hoping for the best: most of the time you can't
use it, but when you can, you're really glad to have it.

[mozilla]: https://support.mozilla.org/en-US/kb/firefox-reader-view-clutter-free-web-pages

I've often wondered how exactly this feature works. This is partially out of
curiosity. Sometimes it gets things slightly wrong, which, sadly, makes me
reluctant to trust it. Its shortcomings suggest something really tricky going
on under the hood. I can't help but wonder where in the pinball machine of
`<div>` tags and CSS class names *that* particular bit of authorial intent got
lost. Sometimes you notice the little Reader View icon in the address bar for
pages that can't plausibly look decent that way, or sometimes it's missing from
pages that by all accounts *should* have it. But in addition to rubbernecking
and taking cheap shots at a brave attempt to address a difficult problem,
there's also a more cooperative motivation: how can I structure the stuff I put
online in a way that will reliably look good in Reader View, and reader modes
in general?

This is, sadly, a poorly-documented topic. Resources consist mainly of
[Stack][so1] [Overflow][so2] [questions][so3], questions on the [Webmasters
Stack Exchange][webmasters], threads on [Mozilla's][mozilla1] [support
forums][mozilla2], now-deleted [blog posts][zumguy], etc. all of which link to
each other. The advice seems to boil down to "It applies a bunch of heuristics
that work pretty well if your HTML is good."

[so1]: https://stackoverflow.com/questions/47822691/how-do-you-create-a-web-page-for-reader-mode
[so2]: https://stackoverflow.com/questions/30661650/how-does-firefox-reader-view-operate
[so3]: https://stackoverflow.com/questions/30730300/optimize-website-to-show-reader-view-in-firefox
[webmasters]: https://webmasters.stackexchange.com/questions/83058/how-do-i-make-my-site-compatible-with-firefoxs-reader-view-feature
[mozilla1]: https://support.mozilla.org/en-US/questions/1067528
[mozilla2]: https://support.mozilla.org/en-US/questions/1140969
[zumguy]: https://web.archive.org/web/20200719050912/http://zumguy.com/enabling-reading-mode-on-your-website/

After some digging I found [this excellent series of 4 articles by Daniel
Aleksandersen from 2018][ctrl-blog] about features like Reader View that
discusses their history, the failed attempts at standardization, an overview of
how these features work and how they differ across implementations, and a plea
to finally standardize the dang thing. The fact that Daniel is writing from
2018 is a little disheartening. A lot of what he writes still feels very
relatable. It's a relatively simple problem, at least compared to what's
typical for the web, so how could we not have made any progress on it in almost
6 years?

[ctrl-blog]: https://www.ctrl.blog/entry/browser-reading-mode-parsers.html

An uncharitable reading of the situation might conclude that Reader View and
features like it are useless, something completely forgotten by browser vendors
and content authors alike, the kind of problem that standardization alone can't
solve. But a quick scan of the [issues][github-issues] and [pull
requests][github-prs] for Readability.js (the tool used for implementing the
Firefox Reader View) suggest that there are at least a *few* people who want
the feature to be good, and some of them even work at Mozilla. From another
perspective, the scattered and wishy-washy advice for how to cooperate with
Reader View is a testament to the feature's overall reliability, an indication
that people are content to accept the situation as-is. If it's not broken, why
fix it? How broken is it actually?

[github-issues]: https://github.com/mozilla/readability/issues
[github-issue-232]: https://github.com/mozilla/readability/issues/232
[github-prs]: https://github.com/mozilla/readability/pulls

To Mozilla's credit, they, like all browser vendors, are up against some pretty
steep odds! These days, most of the HTML on the internet is written with
desktop and mobile users in mind, and occasionally will also consider screen
readers and dark modes. For everything else, you just have to make do with what
you've got. A well known fact about standards is that their mere existence
doesn't mean much on its own, and this plays out on the web with discouraging
regularity. Many of the web's standards, such as CSS media queries, semantic
HTML, ARIA definitions, and even private standards like AMP, feel hopelessly
optimistic when pitted against the chaotic and laissez-faire reality of web
content. In a world where [quirks mode][mdn-quirks-mode] exists, is it anything
but a waste of time to give people the option to ditch the separate "print
view" and instead do it all in one page with HTML and CSS? Maybe some problems
are doomed by human nature to remain unsolved forever, and a reliable reader
mode is one of them.

[mdn-quirks-mode]: https://developer.mozilla.org/en-US/docs/Web/HTML/Quirks_Mode_and_Standards_Mode

Unfortunately, the incentives are rarely aligned between browser vendors and
web content authors (although the situation is improving), and vendors are
significantly outnumbered. There are only a handful of browsers, even including
those with just a small fraction of the total market, and they are motivated to
be secure, robust, and compliant with all the latest standards. Content
authors, however, vary wildly in their goals, and not all of them will
necessarily care (or have the time and resources to care) about things like
whether the page works on a phone, whether "Print to PDF" looks good, whether
CSS grid would have been a better choice than `<table>` (what year is it?),
whether repeated `&nbsp;` is the right way to do indentation, etc. If any of
these things creates a problem for users, realistically it's up to the browser
vendors to do something about it (usually something hacky) since you can't pin
your hopes of capturing market share on content authors doing the right thing:
if CNN looks good in every browser but yours, that's *your* problem, even if
it's really CNN's fault.

And this is why reader modes are interesting to me. The incentives *are*
aligned here. Browser vendors (with the possible exception of Google) want them
to work well, users want them to work well, and, clearly, there are more than a
few web content authors that want them to work well for their sites.
Furthermore, the idea seems pretty aligned with accessibility. So why does the
implementation still feel like a hack? Why has nobody tried to standardize
this? At this point it's clear that reader modes are here to stay, so why not
try to make them good? How is this different from AMP, which for a brief time
was everywhere even though people hated it? How is this different from the Open
Graph protocol? How is this different from Sitemaps or WebP or Flexbox?

The main difference? A standard won't really help. The web community finds
itself in a situation which is all too familiar to software engineers: things
are good *enough*, and the problems aren't a big deal. The only changes to
reader modes that anyone feels are worth the time and energy are the small,
incremental ones that gradually improve the situation, and widespread adoption
of a standard is simply not one of those things. Ultimately, most of the people
who would play nice with a standardized reader mode are already reciting the
`<article>` and `<p>` incantations they got from Stack Overflow.

Furthermore, for the small number of people that want a good reader mode
experience for their website but can't make it work, a standard won't
necessarily help them. Standards can be poorly thought out, can be caught off
guard by broader changes, can be merely a restatement of an existing
implementation, can specify things that never get implemented, etc. Standards
can be bad too, and fixing a broken standard is not an easy task. You're much
more likely to get the `<table>` extraction for your page fixed by submitting
an issue to a GitHub repo (or fixing it yourself, maybe) than by going through
the process of having the standard amended in a way that everyone feels will
fix all `<table>` extraction everywhere for all time.

Cynically, a standard could even make things worse. The whole point of a reader
mode is to *reduce* clutter, and a standard would only provide a jumping-off
point for innovations in content treachery. The effectiveness of things like
search rankings, ad blockers, and tracking prevention rely on website authors
*not* knowing how they work, or not caring enough to know. Perhaps it's a good
thing that reader modes, an obscure, subtly-broken, and poorly-documented
feature, function similarly by pure chance. I can't imagine any ad-supported
websites would be particularly enthusiastic to help browsers show users just
what they came for, with no ads, no links to other articles, no comments
sections, no social media buttons. Imagine the HTML crimes they'd do to sneak
them back in. Imagine the cat-and-mouse game that would ensue. Imagine the
ways they'd meddle in the standardization process.

This is not to suggest that a lack of a standard is a *good* thing, or that
it's a purposeful effort by browser vendors to thwart those who would ruin it
for everybody. But I also don't think the lack of a standard is a *bad* thing
either. Standards are nice to have, but they aren't strictly necessary, and it
seems difficult to create a standard that would benefit users for whom the
current implementation is inadequate, but not those for whom a large population
of reader mode users would be valuable clickbait targets. Perhaps a solution is
possible, but it doesn't seem trivial, both technically and socially, and it
really isn't such a big deal. The reader mode we've got isn't perfect, but it
works. It'll be our little secret.
