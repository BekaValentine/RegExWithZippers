RegExWithZippers
================

The basic idea is to use bizippers for regular expressions as the state space for the NFA that recognizers the regular expression. Let's say we define a simple regular expression language with just characters and concatenation:

    data RegEx = Character Char
               | RegEx :>: RegEx
    
The normal zipper for this is given as regular expressions paired with a context that they appear in. See http://www.haskell.org/haskellwiki/Zipper for more explanation. A bizipper is a zipper paired with a "before" flag or an "after" flag, which indicates whether we're sitting on the left edge of the subexpression before entering it, or on the right edge after exiting it.

    data RegExContext = Top
                      | SeqLeft RegExContext RegEx
                      | SeqRight RegEx RegExContext
    
    data Position = Before | After
    
    data RegExZipper = RegExZipper Position RegEx RegExContext

We then need to define what it means to traverse a regular expression. A transition step in the traversal is a character to read (or no character), together with a next subexpression to go to. A deterministic system would map one subexpression to one subexpression, but a non-deterministic system maps one subexpression to multiple subexpression.

    data Transition = Transition (Maybe Char) RegExZipper
    
    transition :: RegExZipper -> [Transition]
    transition (RegExZipper Before (Character c) con) = [Transition (Just c) (RegExZipper After (Character c) con)]
    transition (RegExZipper Before (r :>: r') con) = [Transition Nothing (RegExZipper Before r (SeqLeft con r'))]
    transition (RegExZipper After r Top) = []
    transition (RegExZipper After r (SeqLeft con r')) = [Transition Nothing (RegExZipper Before r' (SeqRight r con))]
    transition (RegExZipper After r' (SeqRight r con)) = [Transition Nothing (RegExZipper After (r :>: r) con)]

We need to say what we do when we're immediately before a regular expression -- i.e. how do we transition into/over it -- and we need to say what we do when we're after an immediate subexpression of some expression -- i.e. what do we do once we've transitioned over all of the subexpressions?

In the case of a character subexpression we transition over it by recognizing the character, and then ending up on the right of the subexpression. In the case of concatenation, we transition over the whole thing by first transitioning to the left edge of the left concatenated expression.

In the case of being finished with a topmost subexpression, there's nothing to do (it's a/the halting state). When we've finished with a left concatenated expression, we start the right one, and when we've finished with the right one, we've finised with the whole concatenation.

This transition function, together with some generic helper functions, let us define a function `transitionOverSymbol :: Char -> RegExZipper -> [RegExZipper]` which will calculate all the future states that we can reach from some given state by making any number of transitions in the NFA, followed by a single transition that recognizes the given character.

We also need to define some auxiliary functions that let us detect if any given state we're in can transition without recognizing anything and end up in a halting state, which is the state `RegExZipper After r Top` as mentioned before.

    halts :: RegExZipper -> Bool
    halts (RegExZipper Before (Character c) con) = False
    halts (RegExZipper Before (r :>: r') con) = halts (RegExZipper Before r (SeqLeft con r'))
    halts (RegExZipper After r Top) = True
    halts (RegExZipper After r (SeqLeft con r')) = halts (RegExZipper Before r' (SeqRight r con))
    halts (RegExZipper After r' (SeqRight r con)) = halts (RegExZipper After (r :>: r') con)
    
From here it's a simple matter to implement Thompson's algorithm, whereby we consume one character at a time using `transitionOverSymbol` repeatedly, until we end up in at least one halting state.
