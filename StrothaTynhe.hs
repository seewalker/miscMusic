{- Composed by Aphex Twin.
Transcribed by Alex Seewald.
 -}

import Haskore
import Data.Ratio

--A seed-less fold for the implementation of playing piano.
foldl' op (x:xs) = foldl op x xs

playPiano :: [Music] -> [Music] -> Music
playPiano rh lh = (:=:) (foldl' (:+:) rh) (foldl' (:+:) lh)

punctuation :: [Music]
punctuation =
      [ Note(Gf,5) wn [] :=: chord [Note(Ef,4) wn [], Note(Af,4) wn [], Note(Bf,4) wn []],
        chord [Note(Gf,5) wn [], Note(Ef,6) wn []] :=: chord [Note(C,3) wn [], Note(Bf,4) wn [] ],
        chord [Note(Bf,4) wn [], Note(Gf,5) wn []] :=: chord [Note(Gf,3) wn [], Note(Df,4) wn []],
        chord [Note(C,5)  wn [], Note(Af,5) wn []] :=: chord [Note(Af,3) wn [], Note(Ef,4) wn []],
        chord [Note(Df,5) wn [], Note(F,5)  wn [], Note(Df,6) wn []] :=: chord [Note(Df,3) wn [], Note(Df,4) wn []] ]


firstImpression :: Music
firstImpression = playPiano
    --right hand
         [
              --bar1
         chord [Note(A,5) hn [], Note(Gf,6) hn []], chord [Note(F,5) qn [], Note(Df,6) qn []],
         chord [Note(F,5) qn [],Note(Df,6) qn []],
              --bar2
         chord [Note(Ef,6) hn [], Note(Ef,7) qn [] :+: Note(Df,7) qn []],
         Note(Bf,6) hn [],
              --bar3
         chord [Note(Ef,5) qn [], Note(Af,5) qn []], chord [Note(Ef,5) qn [], Note(Af,5) qn []],
         Note(F,5) qn [], Note(Af,5) qn []
         ]
    --left hand
         [
             --bar1
        chord [Note(Gf,4) hn [], Note(C,5) hn []], Note(Bf,3) hn [],
             --bar2
        Note(Gf,4) hn [], chord [Note(Gf,4) hn [], Note(Gf,5) hn []],
             --bar3
        Rest hn, chord [Note(Bf,3) hn [], Note(Df,4) hn [], Note(F,4) hn []]
         ]

magicalRecollection :: Music
magicalRecollection = playPiano
    --right hand
         [
            --bar1
          chord [Note(Af,6) qn [], Note(Df,7) qn []], chord [Note(Af,6) qn [], Note(Df,7) qn []],
          chord [Note(Af,6) hn [], Note(Df,7) hn []],
            --bar2
          chord [Note(G,5) qn [], Note(F,6) qn []], chord [Note(G,5) qn [], Note(Ef,6) qn []],
          chord [Note(G,5) qn [], Note(Ef,6) qn []],
          chord [Note(F,5) qn [], Note(Af,5) qn [], Note(Ef,6) qn []]
         ]
    --left hand
        [
          --bar1
         chord [Note(Bf,4) wn [], Note(Ef,5) wn []],
         --bar2
        Note(Bf,4) wn []
       ]

hesitancyToMeetAgain :: Music
hesitancyToMeetAgain = playPiano
    --right hand
    [
          --bar1
        chord[Note(Df,6) hn [], Note(Af,6) hn [], Note(Df,7) hn []],
        chord[Note(C,6) hn [], Note(G,6) hn []],
          --bar2
        chord[Note(Df,7) qn [], Note(Af,7) qn []], Note(Gf,7) qn [],
        Note(Gf,7) hn [],
          --bar3
        Rest qn, Note(Ef,6) qn [], Note(Gf,5) qn [], Note(E,5) qn []
    ]
    --left hand
    [
          --bar1
       Note(Gf,5) hn [], chord[Note(A,4) hn [], Note(E,4) hn []],
         --bar2
       chord[Note(Df,5) hn [], Note(Gf,5) hn []],
       chord[Note(D,5) hn [], Note(A,5) hn []],
         --bar3
       Note(Af,4) hn [], Note(Df,4) hn []
    ]

apparentRealism :: Music
apparentRealism = playPiano
    --right hand
    [
        --bar1
     chord [ Note(Af,5) qn [], Note(Gf,6) qn []], chord [ Note(Af,5) qn [], Note(Gf,6) qn []],
     chord [ Note(Af,5) qn [], Note(Gf,6) qn []], Note(Ef,6) qn [],
        --bar2
     chord [ Note(F,5) qn [], Note(B,5) qn [], Note(E,6) qn []],
     chord [ Note(F,5) qn [], Note(B,5) qn [], Note(E,6) qn []],
     chord [ Note(F,5) hn [], Note(B,5) hn [], Note(E,6) hn []],
        --bar3
     chord [ Note(Gf,5) qn [], Note(Gf,6) qn []], chord [ Note(Gf,5) qn [], Note(Gf,6) qn []],
     chord [ Note(Df,6) hn [], Note(Df,7) qn [] :+: Note(Bf,6) qn []],
        --bar4
     chord [ Note(D,6) qn [], Note(D,7) qn []], chord [ Note(D,6) qn [], Note(D,7) qn []],
     chord [ Note(D,6) hn [], Note(D,7) qn [] :+: Note(C,7) qn []],
        --bar5
     chord [ Note(Gf,6) qn [], Note(Bf,6) qn [], Note(Ef,7) qn []],
     chord [ Note(Gf,6) qn [], Note(Bf,6) qn [], Note(Ef,7) qn []],
     chord [ Note(Gf,6) hn [], Note(Bf,6) hn [], Note(Ef,7) hn []],
        --bar6
     chord [ Note(Af,5) qn [], Note(Af,6) qn []], chord [ Note(Af,5) qn [], Note(Af,6) qn []],
     chord [ Note(Gf,5) hn [], Note(Ef,6) qn [] :+: Note(Af,6) qn []]
    ]
    --left hand
    [
       --bar1
     chord [Note(Gf,4) dhn [], Note(Df,5) dhn []], chord [Note(Af,4) qn [], Note(Df,5) qn []],
       --bar2
     Note(G,4) wn [],
       --bar3
     Note(Df,4) hn [], Note(Bf,4) hn [],
       --bar4
     chord [Note(G,4) wn [], Note(E,5) wn []],
       --bar5
     chord [Note(Af,4) wn [], Note(Df,5) wn []],
       --bar6
     chord [Note(Gf,4) hn [], Note(Df,5) hn []],
     chord [Note(Df,4) hn [], Note(Af,4) hn []]
    ]

sundering :: Music
sundering = playPiano
    --right hand
    [
       --bar1
     chord [ Note(Df,5) qn [], Note(Af,5) qn []],chord [ Note(Df,5) qn [], Note(Af,5) qn []],
     chord [ Note(Bf,4) hn [], Note(G,5) hn [], Note(Ef,6) hn []],
       --bar2
     Note(D,5) qn [], Note(D,5) qn [], Note(D,5) qn [], Note(D,5) qn [],
       --bar3
     Note(D,5) qn [], Note(D,5) qn [], Note(D,5) qn [], Note(D,5) qn [],
       --bar4
     chord [Note(Df,5) qn [], Note(G,5) qn []],
     chord [Note(D,5) qn [], Note(C,6) qn [], Note(D,6) qn []],
     chord [Note(D,5) hn [], Note(F,5) hn [], Note(D,6) hn []]
    ]
    --left hand
    [
       --bar1
     chord [Note(Df,4) hn [], Note(Gf,4) hn []], Note(Ef,3) hn [],
       --bar2
     chord [Note(C,4) wn [], Note(G,4) wn []],
       --bar3
     chord [Note(D,4) wn [], Note(Af,4) wn []],
       --bar4
     Note(G,3) qn [], chord [Note(F,3) qn [], Note(D,4) qn []],
     chord [Note(D,3) hn [], Note(D,4) hn []]
     ]

--anEncounter inst = Instr inst $ Tempo (6 % 6) firstImpression
anEncounter :: IName -> Music
anEncounter inst = Instr inst $ Tempo (6 % 6)
       (foldl' (:+:) $ zipWith (:+:)
            [firstImpression, magicalRecollection, hesitancyToMeetAgain, apparentRealism,sundering]
            punctuation)

main :: IO ()
main = outputMidiFile "StrothaTynhe.mid" midiGen
    where midiGen = testMidi (anEncounter "acoustic grand")
