# probabilistic-counterpoint

Generation and audio playback of a 1st-species counterpoint against a given cantus firmus, generated probabilistically by leveraging distribution library and randomness.

## Introduction

A cantus firmus is a sequence of notes, all of identical duration, starting and ending on the tonic. In this stripped-down abstraction of a melody, there are no rhythmic, motivic etc. factors. All details are absent except for a bare-bones movement within a scale, starting and ending at a harmonic resting place.

A counterpoint to a cantus firmus is given by
* a musical line with non-trivial melodic movement of its own (i.e. it must be neither too static nor too disjointed)
* which forms consonant intervals with the cantus firmus (dissonant intervals only being allowed under strict rules for their introduction and resolution.)

## Criteria for the counterpoint

There are 2 considerations at play when attempting to generate a counterpoint to a given cantus firmus:
* the leap sizes when moving from each note to the next must be chosen such as to produce a musical line of intrinsic melodic interest (the "horizontal" factor)
* while the intervals formed with the cantus firmus must be consonances (the "vertical" factor.)

The vertical factor is straightforward to control, by simply filtering the array of available notes to choose from, so that only those are available which form a consonant interval with whatever note of the cantus firmus is to be "counterpoint-ed". 

```
getConsonantInKeyPitchesAbove :: Pitch -> [Pitch]
getConsonantInKeyPitchesAbove p@(pc, oct) =
  let twoOctavesAbove =
        [ (pc', oct') | oct' <- [oct..(oct+2)],
                        pc'  <- [(Cff)..(Bss)]]
  in filter
     (\q@(pc, oct) ->
        (arePitchesConsonant q p) &&
        (pc `elem` aMajorPitchClasses)
     )
     twoOctavesAbove
```

As for the horizontal factor, this is less straightforward since a balance must be struck. Too few leaps, or too many small leaps, create an uninteresting musical line with no movement of its own, while too many large leaps result in a broken musical line without melodic integrity.

## Designing the probability distribution

I previously experimented with a distribution that would attempt to strike this balance by trying to make steps (horizontal intervals of size 1, a.k.a. 2nds) and small leaps (3rd / 4ths) more likely, while "size-0 steps" (unisons) and large leaps would get small probabilities on the distribution. Eventually, I decided against this in favor of simply using 2 normal distributions oriented above and below the current note in the counterpoint, in order to form the overall distribution from which the following note should be sampled.