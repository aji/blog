---
title: "Hexbot Part 3: Using Neural Networks"
description: "How MCTS can be augmented with a neural network"
banner: "hex-banner.png"
banner-width: "1400"
banner-height: "700"
---

> This is the third post in a series of posts about my foray into
> building a bot to play Hex. The focus of this post is on how to augment MCTS
> with a neural network, and in particular the AlphaZero-style strategy for
> doing so.
>
> [Hexbot Part 1: The 101 Unit][part-1]  
> [Hexbot Part 2: The Board and Our First Bot][part-2]  
> Hexbot Part 3: Using Neural Networks  
> [Hexbot Part 4: Did It Work?][part-4]

## Improving MCTS

In [part 2][part-2] we went over MCTS, which serves as a tunable baseline for
how to choose moves in a game of Hex: more rollouts means stronger play at the
cost of thinking time, where "strength" grows logarithmically with rollout
count.

We can, of course, do a lot better than basic MCTS, though many paths forward
incorporate domain knowledge about Hex strategy. In [part 1][part-1] we already
mentioned alpha-beta search, a different search algorithm entirely which relies
on a value function, often a hand-crafted one.

MCTS itself can also be improved in a number of ways. The expand step, for
example, can be modified to use *heavy rollouts* where a heuristic is used
instead of choosing moves completely at random. The search step can be augmented
by calculating a *prior* over the possible moves, which influences the weight
assigned to moves with low visit count.

The AlphaZero architecture, which we will be applying to Hex, takes essentially
both of these approaches, using a neural network to augment an MCTS in two ways:

- A **value network** is used instead of a pure rollout, giving each new leaf
  node a number indicating how favorable the network considers the board state
  for either player, and these are aggregated up the tree in the same way as
  rollout outcomes.

- A **policy network** initializes each node with a prior distribution over child
  nodes, i.e. available moves, which biases the search towards moves the network
  deems more likely.

The code I used for this type of search is [on GitHub][github-search], but
it's very similar to the basic MCTS we discussed before just with a different
function to maximize when descending the tree that accounts for the values from
the policy network.

## Improving the neural network

The question of course is how to train these networks. One strategy is to
use expert games as a training set, and this was how AlphaGo was initially
trained. However a key insight of theirs was that the network-augmented MCTS is
an algorithm for taking policy calculations from the model and aggregating them
into a better policy. By having the model play games against itself and training
the model to predict the improved policy returned by MCTS, the model can improve
through self-play. The discovery of AlphaGo Zero is that this idea of
improvement through self-play works *even when starting from a
randomly-initialized network*.[^value]

It may seem a little mysterious that this works at all, and indeed there are a
lot of ways for it to go wrong, but the basic idea makes sense. The model is
essentially learning to predict what an MCTS search will do, and the self-play
is structured to ensure that a diversity of positions are explored and that
the problem is computationally feasible by gradually "compressing" recent
MCTS effort into the model. The model improves the MCTS, and the MCTS improves
the model.

## The model architecture

The model is fundamentally a convolutional neural network, taking an
"image"[^image] of the board and applying a convolution and several residual
convolutional layers to it before taking the final image and flattening it to
be mapped into a policy and value output.

The model size has three configuration parameters:

- `conv_channels`, the number of channels in the intermediate board images.
- `conv_layers`, the number of residual convolutional layers to apply.
- `value_hidden`, the number of hidden neurons in the value calculation.

In pseudo-Python, the model calculation looks like this:

```python
def evaluate(self, board):
    im_input = self.board_to_image(board)

    im = self.conv_input(im_input)
    for (ker0, ker1) in self.residual_conv_layers:
        im = self.residual_conv_layer(im, ker0, ker1)

    policy = self.policy_head(im)
    value = self.value_head(im)

    return policy, value

def board_to_image(self, board):
    # an 11x11 image with 2 channels, one for red pieces
    # and one for blue pieces where a "pixel" is 1.0 if
    # there is a piece and 0.0 otherwise
    return ...

def conv_input(self, im):
    # conv2d with conv_channels 3x3 filters
    return im
        .hex_conv2d(self.conv_input_kernels)
        .batch_norm()
        .leaky_relu()

def residual_conv_layer(self, im, ker0, ker1):
    # conv2d with conv_channels 3x3 filters
    return im
        .hex_conv2d(ker0)
        .batch_norm()
        .leaky_relu()
        .hex_conv2d(ker1)
        .batch_norm()
        .add(im) # skip connection
        .leaky_relu()

def policy_head(self, im):
    # conv2d with two 1x1 filters
    return im
        .hex_conv2d(self.policy_kernels)
        .batch_norm()
        .leaky_relu()
        .flatten()
        .matmul(self.policy_linear) # 242 -> 121
        .log_softmax()

def value_head(self, im):
    # conv2d with one 1x1 filter
    return im
        .hex_conv2d(self.value_kernel)
        .batch_norm()
        .leaky_relu()
        .flatten()
        .matmul(self.value_linear0) # 121 -> value_hidden
        .leaky_relu()
        .matmul(self.value_linear1) # value_hidden -> 1
```

The actual model code is [on GitHub][github-model], and is built on
[Burn][burn], a tensor and autodiff library for Rust. `hex_conv2d` here refers
to a normal convolution but with some of the kernel values set to zero, making
e.g.  a 3x3 kernel only able to incorporate information from cells that are up
to 1 cell away. (I was sort of just winging it when I decided to do this but I
was worried information being able to travel more quickly in one direction might
result in weird behavior.)

This architecture is lifted more or less directly from the
[AlphaGo Zero paper][agz-pdf], with the aforementioned "hex convolution" change.

## Training it

The training procedure at this point is straightforward in concept but full of
many little optimization[^batch] problems[^wgpu] and fiddly implementation[^vulkan] details[^asymm]. I'll go
over the main points quickly here but will leave discussion of smaller points
for another time.

I wrote some code to set up distributed training with components that interact
over HTTP:

- A **controller**, which serves model checkpoints and the self-play log. This
  was just an HTTP server with a light enough workload to run on my laptop.

- A **self-play** daemon, which periodically downloads the latest checkpoint
  and generates self-play data. I ran three instances of this in a Google Cloud
  Run worker pool with NVIDIA L4 GPUs.

- An **optimizer** daemon, which follows the self-play log and runs gradient
  descent on the latest model checkpoint, periodically uploading a new
  checkpoint to the controller. I ran this component on my laptop while at my
  computer and moved it to Cloud Run while away (because running my M4
  unattended at high loads makes me nervous.)

[^asymm]: One interesting thing to point out about training via self-play is
that the self-play portion turns out to be the most computationally intensive
part, and not by a small amount. I used 800 model evaluations per turn, and each
turn results in one new example for the self-play training set. Meanwhile each
iteration of the optimizer is doing only one model evaluation per example in the
minibatch, plus autodiff overhead. It's difficult to quantify the relative value
of generating self-play examples and doing optimizer iterations, but the
asymmetric computational requirements should be clear, and we would hope each
GPU-hour of optimization would be matched by tens or hundreds of GPU-hours of
self-play. Indeed, according to [the AlphaZero paper][az-pdf] they used 64
second-generation TPUs for optimization and 5000 first-generation TPUs for
self-play, a massive FLOPS asymmetry even accounting for the different hardware.
Running the optimizer on my laptop felt much more reasonable when considering
that I had "only" three L4s to use for generating self-play data.

[^wgpu]: A weird problem I ran into is that the performance of the L4s was not
actually very impressive by comparison with my laptop. I assume this is because
I was using `wgpu` in both places, which means the kernels get compiled into
Metal shaders or Vulkan shaders depending on the available hardware, and I
imagine I would have gotten better performance from the L4s if I had used Burn's
CUDA backend instead, but this seemed like enough of a headache to get working
that I punted on it. I would like to come back on this some point and maybe also
look at quantization and other performance tricks, but at the time I mostly
wanted to get a single training run taken care of just to see if things were
moving in the right direction.

[^vulkan]: It turns out that simply installing Vulkan inside a container is not
enough to actually be able to use the NVIDIA L4s attached to a Cloud Run
instance due to the fact that certain configuration files are still missing.
This was a little tricky to figure out and came with an overwhelming feeling of
being well off the beaten path, and I think should have been a strong indicator
that CUDA would have been the right choice.

[^batch]: Keeping the GPU while doing self-play is an interesting performance
challenge. The board image and model outputs need to be sent between the GPU and
CPU for the search to work, which introduces a lot of latency especially for
smaller models. The obvious solution is to batch model evaluations by sending
multiple board states at once, but basic MCTS only looks at one state at a time.
There are strategies for parallelizing MCTS, but these are tricky to implement
in Rust since they involve concurrent edits to a data structure.  Instead my
strategy was to simply run many self-play games concurrently and to send their
evaluation requests to another thread which batches them up for execution on the
GPU. This resulted in significantly more positions per second per GPU, which
means more self-play data generated per second.

I trained the model for about 24 hours with the above setup and the following
model configuration:

```json
{
  "conv_layers": 16,
  "conv_channels": 32,
  "value_hidden": 256
}
```

## Up next

In the [next post][part-4], I'll explain how to decide if the work we've done is
any good.

[part-1]: ./2026-05-10-hexbot-part-1.html
[part-2]: ./2026-05-12-hexbot-part-2.html
[part-4]: ./2026-05-15-hexbot-part-4.html
[az-pdf]: https://arxiv.org/pdf/1712.01815
[agz-pdf]: https://discovery.ucl.ac.uk/id/eprint/10045895/1/agz_unformatted_nature.pdf
[burn]: https://burn.dev/
[github-model]: https://github.com/aji/hex-table/blob/800e2e1566f9a1358c734b1de849336ad592d99b/src/nn/model.rs
[github-search]: https://github.com/aji/hex-table/blob/800e2e1566f9a1358c734b1de849336ad592d99b/src/nn/search.rs

[^value]: This paragraph only describes how the policy network is trained, but
the value network is an equally important part of the model. However, its
training target is rather straightforward: the value network is simply trained
to predict the outcome of the game. An obvious question is why the self-play
data shouldn't use the MCTS-reported value as the training target. I don't know
for sure and imagine there is some reinforcement learning wisdom at play here,
since MCTS *also* takes value calculations and aggregates them into a better
value estimate. But it's worth pointing out that the final outcome is a more
"clean" signal that directly reflects the rules, and as a training target it
lets the information learned at the end of the game affect how *all* earlier
positions are evaluated, instead of each position only seeing as far ahead as
the MCTS was able to get.

[^image]: Not a literal image, but I'll use this term to refer to any
two-dimensional array of vectors. In machine learning the two arguments to a
convolution are often referred to as the image and the kernel, regardless of
whether the image is actually an "image" in the conventional sense. Likewise,
the dimensions of the vectors are referred to as "channels", and I may even
refer to an element of the two-dimensional array as a "pixel" sometimes although
this is a bit of an abuse of terminology in this context.