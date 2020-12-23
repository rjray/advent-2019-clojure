# Breakdown of Files

Here is a breakdown of the various files in this directory. Files with names of the form `dayNN.clj` represent the code actually used to solve the problems (with some tweaking done using a static analysis plug-in for Leinengen). Files with `bis` in the name are modified/tuned versions of the given original day.

The numbers in parentheses in the descriptions of some files represent the rank I had for when my solutions were submitted and accepted.

## day01.clj

Day 1 (1220/1183). The problems (and code) were very simple, so I haven't revisited this file since then.

## day02.clj

Day 2 (1864/1716). This was the first "IntCode" day, where we started building a simple opcode-driven emulator.

## day02bis.clj

A revisit to day 2, in which I replaced the initial IntCode operations with the separate module that was eventually developed. That, and some clean-up to the `p01` and `p02` routines, and this was 21 lines shorter than the original.

## day03.clj

Day 3 (1911/2014). This is the file I'd most like to revisit in the future, as I handled part 2 of the day by mostly just copy/pasting functions from part 1 and making small changes (instead of generalizing the original functions). This is most likely the ugliest of the 25 days, code-wise.

## day04.clj

Day 4 (6229/5838). I also didn't see the need to revisit this one, as the total line-count (with comments) is only 44. Both of the problems were solvable with judicious application of regular expressions.

## day05.clj

Day 5 (2860/2440). The second IntCode day, in which were added 6 new opcodes and a second way of referencing memory by a given opcode. Interestingly, solving the problems themselves required only the use of the VM, no additional code beyond that.

## day05bis.clj

Revision of day 5, with the module version of the IntCode VM. Because of the nature of both problems, this file contains only the `:require` of the IntCode module and the `p01`/`p02` functions. This led to a change in file-size from 173 to 23 lines.

## day06.clj

Day 6 (3142/4787). The first time I would use a graph-search algorithm this season. Upon reviewing some other solutions, I could probably have done part 2 with set operations and saved myself the trouble of the Dijkstra implementation.

## day07.clj

Day 7 (992/1612). The first establishing of the pattern of odd-numbered days being IntCode problems. Two new opcodes were added, and the problems called for the use of the `clojure.math.combinatorics` library. This was the first day in which I was able to solve one of the two problems within the top 1000 finishers.

## day07bis.clj

Revisited, with the substitution of the IntCode module. At this point, the IntCode implementation was getting pretty hefty; little else changed, but the difference in file length was 176 lines.

## day08.clj

Day 8 (1698/1801). This was the first day to incorporate the generation of pixel-based "text" output. Several people solved this using OCR libraries as part of their solutions, but I just had the second problem function display the result to the screen.

## day09.clj

Day 9 (8146/8063). Fourth IntCode problem, and another one where the only code needed (beside the solver functions) was the IntCode VM itself. This time, the VM had a third memory-addressing mode added. This also marked the official "feature-complete" spec for the IntCode VM.

## day09bis.clj

Revisited, with the IntCode module substitution. Because there was no additional shim code between the solver functions and the VM, the file size in lines dropped from 234 to 23.

## day10.clj

Day 10 (1409/1343). This problem involved grouping a series of points by their slope relative to a given reference point. Clojure handled this very well, since ratios (fractions) are first-class entities. There was no need to worry over loss of precision. Part 2 was also the first time I used the `case` macro in Clojure.

## day10bis.clj

Day 10 was also the first non-IntCode day to get a serious revisit, mostly because the way I'd done part 2 with the `case` macro was ugly af. Replacing that with a vector of the only varying parameter and a modulo on the selecting value saved me 19 lines. (Not huge, but I'll take it.)

## day11.clj

Day 11 (6709/6651). Fifth IntCode day, and the last day that I would copy/paste the IntCode implementation. This was also one of the few days (maybe the only one?) where I had to add the ability to have an IntCode VM use mock-input for the sake of testing. This was also a case of generating pixel-based "text", which was just read by myself to answer part 2.

## day11bis.clj

After substituting the IntCode module, this version saved 206 lines. Not much else changed.

## day12.clj

Day 12 (2880/5202). For this day, I learned (amongst other things) to check what a given Clojure library offers, before eventually re-coding something like Least Common Multiple. This was also my first submission of an incorrect answer, as I mis-read part 1 of the problem (passed the wrong value for number of iterations to run). To do LCM, I used some code from my [Project Euler Clojure repo](https://github.com/rjray/projecteuler-clj) for a primes wheel and a prime-factorization function. It solved part 2, but was a little slow. This was my first usage of `defstruct`, `partial`  and `comp` within Clojure.

## day12bis.clj

The main improvement done here was using the `lcm` provided by the `clojure.math.numeric-tower` library. Which I didn't know was there, because I didn't fully read the docs.

## day13.clj

Day 13 (7224/6246). Sixth IntCode day, and the first day in which I started out using a module version of the IntCode implementation from the beginning. My first time using `ref` to create a value that could be changed in multiple places. I also had to re-wire the output mechanism of the VM to allow for a callback rather than a list being stored in the machine's state. This callback was used to update the refs that represented the elements of the game. After looking at some other solutions, I see that I should probably learn about async channels for this sort of thing.

## day13bis.clj

For improving this, I did not get a major jump in line-count since the VM was a module from the outset. Most of the improvement came from changing the six `ref` declarations to a single declaration.

## day14.clj

Day 14 (4233/3639). This day was rather challenging, and I ended up having to go with hints and some examples from another participant into order to solve part 2. My original solution simply had too large of a search-space.

## day15.clj

Day 15 (1417/3430). Seventh IntCode day, second incorrect answer submitted (for part 2 in this case). More semi-Dijkstra-ish search code, but in this case it mapped the whole area instead of stopping when the target was found. In part 2, I mistakenly counted the number of non-wall squares rather than counting the time needed to fill the area. >_<

## day15bis.clj

The only changes made here were the referencing of the `read-opcodes` function that had been moved to the IntCode module (and switching from thread-last to thread-first macros).

## day16.clj

Day 16 (1477/434). This one was tricky, as I started out thinking that Clojure's built-in lazy sequence support would allow me to brute force both parts. I was wrong. For part 2, I had to rely on some hints on reddit threads which eventually led me to a partial-sums approach and algorithm that I was able to adapt into Clojure. Ironically, even with the extra time needed to research this, I got my best position of the event (434) on part 2.

## day17.clj

Day 17 (1703/2745). Eighth IntCode day. While part 1 was pretty straightforward, part 2 required some text-compression techniques to be used. It seems that the puzzle author intended people to use either LZ77 or LZ78, but in the end most people (including myself) hand-encoded the commands for the robot in part 2. This was also the first time I got the sense that my mechanism for feeding input to an IntCode VM instance was hella inefficient.

## day17bis.clj

Very little significant clean-up done this time, aside from using the module version of `read-opcodes`. However, I hope to return to this one at some point and see if I can write code that will derive the series of commands the robot requires AND perform the compression as well.

## day18.clj

Day 18 (2563/2105). The first of three days in which I was unable to finish either part of the day within the first 24 hours. This was a case where part 1 was *significantly* more difficult than part 2 was; once I had part 1 done, modifying it to handle the 4-way split for part 2 was actually pretty simple. But getting the right kind of searching for part 1 took f-ing forever.

## day19.clj

Day 19 (1319/2981). Ninth IntCode day, in which I set aside day 18 and did this one within the initial 24 hour window. That said, I really struggled with part 2 on this one, as might be seen in the gap between my placement for it and for part 1. I also took longer than necessary on part 2 due to having my `x` and `y` values backwards. First use of `memoize`.

## day19bis.clj

Nothing big changed, just pulled out `read-opcodes`.

## day20.clj

Day 20 (3133/2622). Another day that took more than a day to finish. Part 1 wasn't too hard, but part 2 really stumped me for a while. I ended up completely re-imagining the solution, only to decide that I could have done part 1 in roughly the same way which would have made solving part 2 a lot faster. This is another case where there is very little shared code between the two halves of the day.

## day21.clj

Day 21 (965/686). Tenth IntCode, and this may have been the easiest of them. It's the first day in which I finished both halves within the top 1000 finishers. The hardest part was coming up with the Boolean logic expressions; once I had those feeding them to the "robot" reused code from day 17. Part 2 went quite quickly once part 1 was done.

## day21bis.clj

Mostly similar changes as the last few IntCode days (`read-opcodes`, `->` vs. `->>`, etc.). This time also removed a function that displayed the consumed output since it wasn't really needed.

## day22.clj

Day 22 (4029/1967). The last of the really hard days. I might have gotten at least part 1 done within 24 hours, but I was traveling to another state during most of the period which threw off my timing. This was another case of part 1 being perfectly do-able with a direct (i.e., brute-force) approach while part 2 was so not. Getting part 2 required some major hints from reddit. I also ended up having to get a Clojure version of "modular exponentiation", to avoid overflowing even the bigint support.

## day23.clj

Day 23 (1155/1203). Eleventh IntCode, and again fairly easy (especially compared to day 22). I was able to handle some of the logic in shared code, with extra stuff for part 2.

## day23bis.clj

Just the now-usual changes.

## day24.clj

Day 24 (667/1695). Ah, a variation on Conway's Game of Life. With quite a tricky change for part 2. Part 1 was straightforward-enough to finish in the top 1000, but the recursive nature of part 2 took a while to get right. At least one of my wrong submissions came on this day.

## day25.clj

Day 25 (635/475). Twelfth IntCode day, the last day of the series, and the most annoying of the puzzles. For this, we had to "solve" a miniature Zork-like text adventure. It came down to trying various combinations of six different objects in order to be able to enter the final room. I found it irritating, because it didn't really lend itself well to a "fire and forget" solution. Unlike the previous 24 days, this seemed to **require** that you sit and run it interactively. There were participants who managed to code enough language-parsing to automate the process, but in the end I just loaded up `clojure.math.combinatorics` in a second REPL and used that to generate the combinations from one-at-a-time to four-at-a-time and brute-forced them until one finally worked. Still, it got me in the top 1000 for part 1. And this being day 25, there was no real puzzle for part 2. Instead, you were rewarded for having all 49 previous stars. Which apparently a lot of people didn't, since I was able to "finish" part 2 in the top 500.

## day25bis.clj

Mostly familiar updates here, though I did streamline the function that fed input lines into the IntCode machine.

## heap.clj

This is a combination min-heap/max-heap module that I used in last year's AoC. It was originally written as part of the Coursera Data Structures specialization.

## intcode.clj

This is where I extracted the IntCode "VM" to, and eventually tuned various parts of it.
