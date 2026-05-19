---
title: "Hexbot Part 2: The Board and Our First Bot"
description: "Representing the board state and building a simple MCTS-based bot"
banner: "hex-banner.png"
banner-width: "1400"
banner-height: "700"
---

> This is the second post in a series of posts about my foray into
> building a bot to play Hex. The focus of this post is on the bitboard
> implementation and an MCTS-based bot using it, as well as some analysis of the
> MCTS algorithm's relative strength for different iteration counts.
>
> [Hexbot Part 1: The 101 Unit][part-1]  
> Hexbot Part 2: The Board and Our First Bot  
> [Hexbot Part 3: Using Neural Networks][part-3]  
> [Hexbot Part 4: Did It Work?][part-4]

## The bitboard

The first step in implementing a bot for any board game is to have logic and
data structures for the game itself. For Hex, as with any game, there are a
variety of approaches that would work here, but for our purposes, and for MCTS
especially, we want this code to be as lightweight as possible.

To start, let's consider how to represent a hexagonal grid on a computer in the
first place. Luckily for us, the grid for Hex is a square that's been skewed
into a rhombus. We can simply represent the data in memory as a square and then
skew or un-skew as appropriate, e.g. for displaying the board:

```text
AA  AB  AC  AD  AE  AF  AG  AH  AI  AJ  AK
  BA  BB  BC  BD  BE  BF  BG  BH  BI  BJ  BK
    CA  CB  CC  CD  CE  CF  CG  CH  CI  CJ  CK
      DA  DB  DC  DD  DE  DF  DG  DH  DI  DJ  DK
        EA  EB  EC  ED  EE  EF  EG  EH  EI  EJ  EK
          FA  FB  FC  FD  FE  FF  FG  FH  FI  FJ  FK
            GA  GB  GC  GD  GE  GF  GG  GH  GI  GJ  GK
              HA  HB  HC  HD  HE  HF  HG  HH  HI  HJ  HK
                IA  IB  IC  ID  IE  IF  IG  IH  II  IJ  IK
                  JA  JB  JC  JD  JE  JF  JG  JH  JI  JJ  JK
                    KA  KB  KC  KD  KE  KF  KG  KH  KI  KJ  KK
```

We also define the red player as the one attempting to connect the left and
right edges and the blue player as the one attempting to connect the top and
bottom edges.

Bitboards are one class of board representation that use bitmap layers to
represent various features of the game. This works nicely for Hex because
each of the 121 cells of an 11x11 board can be in one of 3 states. We'll use
two `u128`s, one for the red pieces and one for the blue pieces:

```rust
#[derive(Copy, Clone)]
struct Bitboard {
    pub red: u128,
    pub blue: u128,
}
```

With some bits left over, we can also choose one of the unused bits to represent
the player whose turn it is to move:

```rust
const NEXT_MOVE: u128 = 1 << 127;

fn next_move(&self) -> Player {
    match self.red & NEXT_MOVE != 0 {
        true => Player::Red,
        false => Player::Blue,
    }
}
```

This struct is only 32 bytes, making it very cheap to copy around, and we can
even derive a `Copy` impl for it. With this representation, the 121 least
significant bits of `red` represent cells containing a red piece, and likewise
for `blue`. (Strictly speaking it's possible for both planes to have a 1 in the
same position, but we'll simply assume this never happens.) A function for
mapping a row and column to a cell on the board is rather simple:

```rust
fn rc_mask(&self, r: usize, c: usize) -> u128 {
    1 << (120 - r * 11 - c)
}

fn rc(&self, r: usize, c: usize) -> Option<Player> {
    let mask = self.rc_mask(r, c);
    let is_red = (self.red & mask) != 0;
    let is_blue = (self.blue & mask) != 0;
    match (is_red, is_blue) {
        (true, _) => Some(Player::Red),
        (_, true) => Some(Player::Blue),
        _ => None,
    }
}
```

We can set cells and do other basic manipulations in a similar manner, and
anyone familiar with bit manipulation should feel quite comfortable with the
ideas expressed so far. Queries such as calculating the number of available
moves should also be fairly obvious at this point (look up `popcount` if you're
stuck).

The more interesting problem in front of us is how to check whether the game is
over, i.e. which player has connected the two edges. This brings us to another
strength of bitboards: we can represent a translated board state using bit
manipulation. In our case, we want to be able to translate the board by one
cell in each of the 6 directions. The basic premise for any given direction
is to use a mask to select the bits that will move then use a bit shift to move
them to their new locations. The reason this works is because the bit offset
between the cell at $(r,c)$ and the cell at $(r+dr,c+dc)$ is the same for all
such pairs of cells, $dr\times 11 + dc$.

To check whether a player has connected their edges, we start with the cells
on one edge, traverse the graph of adjacent cells of their color, then check
if the traversal reached any cells on the opposite edge. The function that
implements the graph traversal on bit sets is called `bb_fill` by analogy with
flood filling:

```rust
fn bb_fill(start: u128, traversable: u128) -> u128 {
    let mut cur = start & traversable;
    loop {
        let mut next = cur;
        next |= (cur & ADJ0_KEEP) >> ADJ0_SHR;
        next |= (cur & ADJ1_KEEP) >> ADJ1_SHR;
        next |= (cur & ADJ2_KEEP) >> ADJ2_SHR;
        next |= (cur & ADJ3_KEEP) << ADJ3_SHL;
        next |= (cur & ADJ4_KEEP) << ADJ4_SHL;
        next |= (cur & ADJ5_KEEP) << ADJ5_SHL;
        next &= traversable;
        if next == cur {
            return cur;
        }
        cur = next;
    }
}
```

Where `ADJn_KEEP` and `ADJn_SHd` are masks and offsets for translations as
described above. `cur` stores the set of currently reachable cells, and the loop
body translates this set in all 6 directions and masks it with `traversable` to
calculate the new set of reachable cells. This process is repeated until no new
cells are added to `cur` and returns the full set of cells that are reachable
from `start` through `traversable`. With this function, checking if a player has
won is fairly straightforward:

```rust
const RED_START = ...; // left edge bits
const RED_END = ...; // right edge bits
const BLUE_START = ...; // top edge bits
const BLUE_END = ...; // bottom edge bits

fn win(&self) -> Option<Player> {
    let r = bb_fill(RED_START, self.red) & RED_END;
    let b = bb_fill(BLUE_START, self.blue) & BLUE_END;
    match (r != 0, b != 0) {
        (true, _) => Some(Player::Red),
        (_, true) => Some(Player::Blue),
        _ => None,
    }
}
```

And for a basic Hex board, that's really all there is to it! Bitboards are still
used for games with much more complex rules (I've even
[written one for Khet][github-khet-bb]), but the simplicity of Hex works
tremendously in our favor and makes an efficient bitboard possible with very
little code.

The full code for the bitboard module is [on GitHub][github-bb]. (The actual
code differs from the code in this post in a number of more or less trivial
ways, such as "black and white" instead of "red and blue" and `bool`
instead of `Player`, but the basic idea is the same.)

## Monte Carlo tree search

As discussed in [part 1][part-1], a basic MCTS requires very little
domain-specific code beyond what have in our bitboard, i.e. a way to enumerate
valid moves and check if the game has ended. Described briefly, the algorithm
is as follows:

> An MCTS search tree contains a node for each visited state, where the node's
> children represent states reachable by making a valid move from that state.
> The edges from state $s$ to $a$ track various statistics:
> 
> - $N(s,a) =$ the number of times $(s, a)$ has been traversed during the search.
> - $V(s,a) =$ the total value of actions in the subtree rooted at $a$.
> 
> Each iteration of an MCTS search proceeds as follows:
> 
> 1. **Select.** Starting from the root, descend the move tree until hitting a
> node $L$ which hasn't been seen yet. From a node $s$, choose the child node $a$
> which maximizes a function $\mathrm{select}(s, a)$. One popular choice is called
> Upper Confidence on Trees (UCT):
>
>     $$\mathrm{UCT}(s, a) = \frac{V(s, a)}{N(s, a)} + C \sqrt{\frac{\ln N(s)}{N(s, a)}}$$
> 
> 2. **Expand.** Evaluate $L$ with a *rollout*, playing random moves until the
> game ends. The winner determines the value $v$ which will be used in the next
> step.
> 
> 3. **Backpropagate.** Returning to the root of the tree, update
> $V(s,a) = V(s,a) + v$ and $N(s,a) = N(s,a) + 1$, altering $v$
> as appropriate depending on whose turn it is to play from $s$.
> 
> Once enough iterations have been performed, the $a$ that maximizes
> $N(\mathrm{root},a)$ is played.

For a more in-depth explanation of MCTS, there are a variety of helpful
resources online and I won't try to outdo them here. The
[Wikipedia article][wiki-mcts] and [Chess Programming Wiki article][chess-wiki-mcts]
provide a good starting point for more information.

The baseline MCTS implementation used [in the code][github-mcts] more or less
follows the algorithm described above, but with one important optimization
which is worth pointing out. A naive rollout would play randomly and check the
win condition, but for Hex we can make a much faster and much more efficient
approximation, once again owing to the simplicity of Hex. We know that every
additional move selects an unoccupied cell, and that a completely filled board
is guaranteed to be a terminal state.  Thus, in a single step, we can perform an
MCTS rollout by assigning the empty cells randomly and checking who has won:

```rust
fn mcts_rollout(&self) -> Player {
    let empty = self.empty_bits();
    let mask = rand::random::<u128>();
    let red = self.red | (mask & empty);
    match bb_fill(RED_START, red) & RED_END != 0 {
        true => Player::Red,
        false => Player::Blue,
    }
}
```

This code assigns a random subset of empty cells to red and checks if this is a
winning placement of red cells. If not, its complement must be a winning
placement of blue cells. Note that this is not *strictly* correct, since it
will likely result in too many or too few cells being assigned to red. It's
equvialent to making random moves in a game where players flip a coin to decide who plays next.
However, this difference does not significantly reduce the effectiveness of
MCTS, and the amount of computation saved is well worth it. (An experiment
worth doing would be to compare the relative strength of "proper" rollouts, but
I haven't taken the time to do this.)

## Measuring the relative strength of MCTS

An MCTS search can be stopped at any time for any reason and return a result.
This lets us use *rollout count* (i.e. iteration count) as a way of configuring
or measuring the "strength" of an MCTS search. In practice, such as when playing
with a clock, the decision of how much computation to spend per move becomes an
important strategic one, but a fixed rollout count per move works well as a
simple reference point.

What we would like is to model the probability that an MCTS playing as red with
$n$ rollouts wins against an MCTS playing as blue with $m$ rollouts. A simple
model is to use logistic regression with $\log n - \log m$ as the independent
variable. This is essentially the same idea as the Elo rating system, where
$\log n$ plays the role of the rating. Logistic regression lets us calibrate
our rating system so we can turn rollout count ratios into win probabilities.
The choice of $\log n$ instead of simply $n$ or some other function of $n$ is
motivated by two important assumptions: $\log n$ correlates with the average
depth of the search tree, and the depth of the search tree correlates with
strength. An effect of this assumption is that only the *ratio* of computational
effort matters: thinking 20x as long is assumed to be equally strong no matter
the absolute amount of computational effort invested.

By generating pairs $(n, m)$ (sampled so $\log n$ and $\log m$ are uniform and
i.i.d.) and running games, we can generate a dataset of outcomes and perform
logistic regression to calibrate our rating system. Something like the following
turns out to be a decent predictor:

```rust
fn p_red_win(red_rollouts: usize, blue_rollouts: usize) -> f64 {
    let red_rank = (red_rollouts as f64).log10();
    let blue_rank = (blue_rollouts as f64).log10();
    let x = red_rank - blue_rank;
    (0.3 + 1.8 * x).sigmoid()
}

fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}
```

(The mathematically inclined will notice that I've got some terms with an
$\mathrm{exp}({\log m - \log n})$ shape here meaning we can simplify to a
ratio of powers of $m$ and $n$ but I'll leave that as an exercise.)

The [code][github-rank-code] and [notebook][github-rank-notebook] for this
analysis are on GitHub.

## Up next

In [the next post][part-3], I'll talk about how we can use an AlphaZero-style
neural network to significantly improve the effectiveness of MCTS.

[part-1]: ./2026-05-10-hexbot-part-1.html
[part-3]: ./2026-05-13-hexbot-part-3.html
[part-4]: ./2026-05-15-hexbot-part-4.html
[github-khet-bb]: https://github.com/aji/khet-table/blob/main/src/bb.rs
[github-bb]: https://github.com/aji/hex-table/blob/main/src/bb.rs
[github-mcts]: https://github.com/aji/hex-table/blob/main/src/mcts.rs
[github-rank-code]: https://github.com/aji/hex-table/blob/main/src/bin/mcts.rs
[github-rank-notebook]: https://github.com/aji/hex-table/blob/main/notebooks/skill.ipynb
[wiki-mcts]: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
[chess-wiki-mcts]: https://www.chessprogramming.org/Monte-Carlo_Tree_Search