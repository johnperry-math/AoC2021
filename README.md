# Advent of Code 2021 in Ada

Because 3 years of pain and suffering aren't enough. :grin:
I did at least use Alire first, as well as a custom-made `Queue`.

## Days I completed on the first try, without even doing the example first

### Day 1: Sonar Sweep

Study the sea floor. You receive a number of sonar readings of bottom depth.

1. Count the number of times the depth increases.
1. Count the number of times that the sum of three consecutive depths increases.

#### Tools
Nothing special.

#### Experience
Fun and easy.

### Day 2: Dive!

You're given a bunch of commands for motion forward, up, or down.

1. Predict the sub's position.
1. You misinterpreted up and down: they aren't position, but _aim_.
   Predict now, with this information.

#### Tools
Nothing special.

#### Experience
Fun and easy.

### Day 3: Binary Diagnostic

You're given a bunch of binary numbers.
Perform diagnostics by looking at the most and least commonly-used bits.

1. Power consumption is product of gamma rate (most common bit) and
   epsilon rate (least common bit). Report it.
1. Life support rating is product of
   * oxygen generator rating, the number determined by the most common bits, and
   * carbon dioxide scrubber rating,
     the number determined by the least common bit.

   Report them.

#### Tools
* Custom `String` type for the diagnostic.

#### Experience
Fun and easy. The implementation won't work with the example input
because the string lengths are hard-coded for the puzzle input.
I could fix it, and if I find motivation then I will.

## First mistake :-( started doing examples after this

### Day 4: Giant Squid

A giant squid comes by and you decide to play bingo.
You are given a sequence of numbers that will be called, and a bunch of cards.

1. Which card will win bingo first? report the sum of its unmarked values.
1. Scratch that; the squid looks intimidating.
   Which card will win bingo _last?_ report the sum of its unmarked values.

#### Tools
* Ada's `Vector` type
* a pragma to hush a warning

#### Experience
But for the fact that I did it wrong, this was fun.

## Days I completed after doing the example first

### Day 5: Hydrothermal venture

A bunch of hydrothermal vents.
Oddly _not_ related to [Day 9](#day-9-smoke-basin).

1. Count the number of overlapping vents from those
   that line up horizontally or vertically.
1. Now count the number that add up diagonally, as well.

#### Tools
* Ada's `Vector` and `Ordered_Map` types.

#### Experience
Surprisingly uncomplicated. Quick.

### Day 6: Lanternfish

You pass some lanternfish, which are bioluminescent.
They grow according to energy levels.

1. How many do we expect to have after 80 days?
1. ...after 256 days?

#### Tools
* Ada's `Vector` type

#### Experience
This took a while because I was trying to work out a formula
alon the lines of a Lucas sequence,
of which the Fibonacci sequence is a special case;
traces of this remain in my use of the term `Lanternfishonacci` somewhere.

After squandering far too much time on that step,
I realized it wasn't gonna happen, but I also knew that
I _really_ didn't want to track individual fish, as the example suggested.
That's when I hit on the idea of tracking not individual fish,
but the number of fish, along with the number that spawned on a given day.
That worked, and was fast!
It also showed that I was right: the number of individual fish
is enormous.

### Day 7: The Treachery of Whales

A whale is chasing you, and crabs come to your help.
No, the anti-whale message ;-) doesn't make sense to me,
especially since you just passed an enormous school of tasty lanternfish
who are in desperate need of a predator to cull their numbers.
Nor does it seem to matter much for the story; the whale is soon forgotten,
but the crab bit makes some sense since their little ships
can travel only horizontally, and that's the focus of the problem.

1. Find the lowest- position where the crabs should align their ships.
1. Same, but the cost is different: each additional position to move
   costs one additiona fuel, rather than all positions being 1 fuel.

#### Tools
* Ada's `Vector` type
* an enumerated type to have a subprogram behave differently
  for different parts of the puzzle
* Mathematics! in particular, triangular numbers:
  the sum of the numbers from 1 through n is [n(n+1)]/2

#### Experience

Easy and fun. The trick with triangular numbers was a pleasure.

### Day 8: Seven Segment Search

Some displays have their wires crossed.
You have notes with 10 input signals, each of which is a unique digit,
and 4 output displays, each of which is incorrect.

1. Determine how many output displays correspond to numbers 1, 4, 7, 8,
   which we can identify uniquely by the number of their segments.
1. Determine the correct wirings and thus the correct outputs.
   Report the sum.

#### Tools
* custom character range
* Ada's `Vector`
* a pragma to shut up a warning about an unused subprogram

#### Experience

Tedious. It took me a long time to work out the correct wirings.
It wasn't hard after that, though I had a couple of false starts,
and had trouble implementing it correctly.
I don't seem to have been the only one; the leaderboard suggests
that this day took most people much longer than normal --
though the difference here is something like 7 minutes versus 1-4 minutes,
while in my case it was more like 4 hours versus 1...

### Day 9: Smoke Basin

Traveling through an area where lava tubes are releasing smoke and it...
flows _downward?!?_
The basin has high points and low points.

1. Determine the low points, calculate their risk levels, and add 'em up.
1. Find the sizes of the basins surrounding the low points.
   Report the product of the sizes of the three largest.

#### Tools
* Ada's `Hashed_Sets` type.
* My `Queue` type.

#### Experience
Not hard, not tedious. Took longer than it should have
because I used a temporary variable rather than an similarly-named record field,
but otherwise it was fun!

### Day 10: Syntax Scoring

A bunch of messages that consist entirely of opening and closing delimiters,
but they're either corrupt or incomplete.

1. determine which ones are corrupt; report syntax error score
1. complete the incomplete ones; report autocomplete score

#### Tools
* Vectors
* enumerated types
* recursion (it's a grammar, after all)

#### Experience
Not especially hard, nor tedious.
Took longer than it should have because I was a little careless.

### Day 11: Dumbo Octopus

Look at how the octopus are growing in energy; they flash at 9, so

1. determine how many flashes occur in 100 rounds
1. determine the round when they all flash

#### Tools
* Breadth-first search to determine flash spillover, which means...
* My `Queue` type

#### Experience
Not particularly challenging, not particularly tedious.

#### Visualization
This lends itself to visualization quite well.

[<img src="day11/full_thing.gif" width="240">](day11/full_thing.gif)

### Day 12: Passage Pathing

Searching for all the paths through a chambered cavern.
There are two types of chambers: small (lower-case) and large (upper-case).

1. Determine the number of paths that have _no_ repetition of small chambers.
1. Determine the number of paths that have _at most one_ repetition
   of _one_ small chamber which is neither `start` nor `end`.

#### Tools
* Ada's `Vector`, `Hashed_Maps`, `Hashed_Sets`, and `Ordered_Maps`.
* Breadth-first search for paths, which means...
* My `Queue` type.

#### Experience
Getting the program to work with the examples got it to work with the input,
but it was a challenge to get it to work with the examples!
Getting set up for Part 1 was already tedious, and
while Part 2 should have been relatively quick after that,
I kept getting the logic wrong.
Worse, it's slow: part 2 takes about 45 seconds!
A first attempt at optimizing did not work;
I now suspect it's the use of `Ordered_Maps`,
but I'm done with the problem for now.
It's a nice problem! I just have to sleep at some point...

### Day 13: Transparent origami

You need to figure out the code to initialize a device
by performing a number of folds on a sheet of paper
that has seemingly random dots.
Once you do that, the initialization code should be 8 capital letters.

1. Perform only the first fold and report the number of dots.
   Do not double-count dots that overlap.
1. Perform the remaining folds and report the letters that appear.

#### Tools
* Ada's `Vectors` and `Hashed_Sets` types
* the `Get_Integer` function I wrote for Advent of Code 2022
* Mathematics! in particular, some elementary geometry of transformations

#### Experience
Fun and easy. I had to do a little debugging because I forgot in part 2
to switch away from performing the folds on `Dots` to `New_Dots`.

### Day 14: Extended Polymerization

You need to manufacture more polymer to reinforce the submarine's hull.
You are given a polymer template and rules for inserting an element
between each pair of elements.
We score the polymer by computing the difference
between the highest and lowest frequencies of elements.

1. Determine the score after 10 turns. This is a reasonable number.
1. Determine the score after 40 turns. This is an unreasonable number,
   and you won't be able to compute it directly.

#### Tools
* Ada's `Vector` and `Hashed_Map` types.
* For part 2, the `Hashed_Map` comes in quite handy.
  Instead of trying to compute the polymer directly:
  * count the number of pairs in the initial polymer;
  * then, on each turn, use that count to count the number of new pairs.

#### Experience
Mostly fun. Not quite so easy. It took me a few minutes to figure out
how to re-imagine the problem away from computing the vector directly.
Then it took me a while to debug the code.

Ada's rules on tampering with cursors are so strict that they flagged my code
even though _I was doing nothing dangerous._
Rather irritating.
