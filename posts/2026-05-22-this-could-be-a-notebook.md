---
title: "This Could Be a Notebook"
description: "Jupyter notebooks are a cool format, but could be cooler"
---

I've been using Jupyter notebooks for a while now, mostly for small things that
are the kind of thing Jupyter is good at: loading some data, processing that
data, plotting some stuff, going back and redoing stuff differently, etc. I like
that cells can produce rich outputs, even things like progress bars or
interactive plots. I also like being able to include Markdown cells, which feels
like it achieves what literate programming was meant to.

I keep trying to think up ways to use notebooks for other things in a way that
actually adds value and isn't just a tech demo, and so far my attempts have
been unsuccessful. I got a decent part of the way through setting up Hakyll so
I could use `.ipynb` files as posts on this blog, but the rate at which jank was
accumulating made me give up on that idea pretty quickly. (I still think this
is practical, it just has some rough spots that need to be worked out first.)
I've experimented with using notebooks (with both Python and Deno kernels) for
simple ops tasks, and keep feeling like a script would do the job better on
too many axes. I do think there's value in using notebooks as a format for
things like analyzing logs, but this is just an ops-flavored version of a task
we already know notebooks are good for, with CSVs swapped for JSONL files.

Maybe there's not actually a problem here, but I think the raw material of the
notebook workflow has a lot of potential for ops work. To me, the following
points are critical limitations of the status quo:

- Cells can be run out of order, multiple times, etc. This is an important part
of exploration and experimentation, but makes notebooks pretty ineffective at
ensuring a series of steps are executed in a specific order a specific number
of times.

- Re-running a cell erases the output of previous executions. This means a
notebook doesn't tell you what was *actually* done, only what was done last.

## What I would do differently

I'm imagining a notebook-based tool that enables the following style of
automation:

- A workflow is generated from a parameterized notebook.

- The workflow executes one cell at a time.

- The workflow stops on failed cells. The operator can choose to either re-run
the cell or cancel the workflow. If retrying a failed cell, the workflow will be
marked as having gone off script.

- The operator can edit cells, re-run successful cells, or run cells out of
order. Any of these will also mark the workflow as having gone off script.

- The exact sequence of cells that were run is logged, with timestamps.

This would be a relatively small addition to current notebook tooling,
essentially being a "restricted mode" for notebook execution with some
additional logging. It has a number of properties that I think make it an
interesting possibility for ops work:

- The ability to include rich outputs such as graphs, tables, etc. is useful
both during and after workflow execution.

- Auth is consolidated, since all actions are performed by the notebook kernel
on behalf of the operator. There's no need to think about SSH keys, API tokens,
web UI access, etc. Such a tool could also conceivably allow notebooks to be
executed with a particular role, allowing finer grained control over what
actions the notebook is allowed to perform.

- Logging all cell executions provides both valuable audit information as well
as a good starting point for investigating why a workflow went off script and
how it might need to be changed.

- If `.ipynb` files can be imported as new workflows, then developing a new
workflow is as simple as opening the notebook editor of your choice and getting
to work. (Although this workflow tool as described would need to include all
the functionality necessary for a notebook editor, and in the context of the
aforementioned auth point, it might make sense to write new workflows directly
in tool.)

I'm gonna have a look around and see if there's anything like this out there.