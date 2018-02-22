{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module NameGen where

import           Test.QuickCheck
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import Name

newtype ArbitraryName = ArbitraryName { arbitraryName :: Name }

arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

instance Arbitrary ArbitraryName where
    arbitrary = do
        s:str <- listOf1 arbitraryPrintableChar
        return $ ArbitraryName $ fromNonEmpty (s :| str)

arbitraryReadableName :: Gen Name
arbitraryReadableName = undefined

adjectives :: [NonEmpty Char]
adjectives = 
    [ 'a' :| "dmiring"
    , 'a' :| "doring"
    , 'a' :| "ffectionate"
    , 'a' :| "gitated"
    , 'a' :| "mazing"
    , 'a' :| "ngry"
    , 'a' :| "wesome"
    , 'b' :| "ackstabbing"
    , 'b' :| "erserk"
    , 'b' :| "ig"
    , 'b' :| "oring"
    , 'c' :| "lever"
    , 'c' :| "ocky"
    , 'c' :| "ompassionate"
    , 'c' :| "ondescending"
    , 'c' :| "ranky"
    , 'd' :| "esperate"
    , 'd' :| "etermined"
    , 'd' :| "istracted"
    , 'd' :| "reamy"
    , 'd' :| "runk"
    , 'e' :| "ager"
    , 'e' :| "cstatic"
    , 'e' :| "lastic"
    , 'e' :| "lated"
    , 'e' :| "legant"
    , 'e' :| "vil"
    , 'f' :| "ervent"
    , 'f' :| "ocused"
    , 'f' :| "urious"
    , 'g' :| "igantic"
    , 'g' :| "loomy"
    , 'g' :| "oofy"
    , 'g' :| "rave"
    , 'h' :| "appy"
    , 'h' :| "igh"
    , 'h' :| "opeful"
    , 'h' :| "ungry"
    , 'i' :| "nfallible"
    , 'j' :| "olly"
    , 'j' :| "ovial"
    , 'k' :| "ickass"
    , 'l' :| "onely"
    , 'l' :| "oving"
    , 'm' :| "ad"
    , 'm' :| "odest"
    , 'n' :| "aughty"
    , 'n' :| "auseous"
    , 'n' :| "ostalgic"
    , 'p' :| "eaceful"
    , 'p' :| "edantic"
    , 'p' :| "ensive"
    , 'p' :| "rickly"
    , 'r' :| "everent"
    , 'r' :| "omantic"
    , 's' :| "ad"
    , 's' :| "erene"
    , 's' :| "harp"
    , 's' :| "ick"
    , 's' :| "illy"
    , 's' :| "leepy"
    , 's' :| "mall"
    , 's' :| "toic"
    , 's' :| "tupefied"
    , 's' :| "uspicious"
    , 't' :| "ender"
    , 't' :| "hirsty"
    , 't' :| "iny"
    , 't' :| "rusting"
    , 'z' :| "en"
    ]

scientists :: [NonEmpty Char]
scientists = 
    [ -- Muhammad ibn Jābir al-Ḥarrānī al-Battānī was a founding father of
      -- astronomy.
      -- https:--en.wikipedia.org/wiki/Mu%E1%B8%A5ammad_ibn_J%C4%81bir_al-%E1%B8%A4arr%C4%81n%C4%AB_al-Batt%C4%81n%C4%AB
      'a' :| "lbattani"

      -- Frances E. Allen became the first female IBM Fellow in 1989. In 2006
      -- she became the first female recipient of the ACM's Turing Award.
      -- https:--en.wikipedia.org/wiki/Frances_E._Allen
    , 'a' :| "llen"

      -- June Almeida - Scottish virologist who took the first pictures of the
      -- rubella virus - https:--en.wikipedia.org/wiki/June_Almeida
    , 'a' :| "lmeida"

      -- Maria Gaetana Agnesi - Italian mathematician philosopher theologian
      -- and humanitarian. She was the first woman to write a mathematics
      -- handbook and the first woman appointed as a Mathematics Professor at a
      -- University. https:--en.wikipedia.org/wiki/Maria_Gaetana_Agnesi
    , 'a' :| "gnesi"

      -- Archimedes was a physicist engineer and mathematician who invented too
      -- many things to list them here.
      -- https:--en.wikipedia.org/wiki/Archimedes
    , 'a' :| "rchimedes"

      -- Maria Ardinghelli - Italian translator mathematician and physicist -
      -- https:--en.wikipedia.org/wiki/Maria_Ardinghelli
    , 'a' :| "rdinghelli"

      -- Aryabhata - Ancient Indian mathematician-astronomer during 476-550 CE
      -- https:--en.wikipedia.org/wiki/Aryabhata
    , 'a' :| "ryabhata"

      -- Wanda Austin - Wanda Austin is the President and CEO of The Aerospace
      -- Corporation a leading architect for the US security space programs.
      -- https:--en.wikipedia.org/wiki/Wanda_Austin
    , 'a' :| "ustin"

      -- Charles Babbage invented the concept of a programmable computer.
      -- https:--en.wikipedia.org/wiki/Charles_Babbage.
    , 'b' :| "abbage"

      -- Stefan Banach - Polish mathematician was one of the founders of modern
      -- functional analysis. https:--en.wikipedia.org/wiki/Stefan_Banach
    , 'b' :| "anach"

      -- John Bardeen co-invented the transistor -
      -- https:--en.wikipedia.org/wiki/John_Bardeen
    , 'b' :| "ardeen"

      -- Jean Bartik born Betty Jean Jennings was one of the original
      -- programmers for the ENIAC computer.
      -- https:--en.wikipedia.org/wiki/Jean_Bartik
    , 'b' :| "artik"

      -- Laura Bassi the world's first female professor
      -- https:--en.wikipedia.org/wiki/Laura_Bassi
    , 'b' :| "assi"

      -- Hugh Beaver British engineer founder of the Guinness Book of World
      -- Records https:--en.wikipedia.org/wiki/Hugh_Beaver
    , 'b' :| "eaver"

      -- Alexander Graham Bell - an eminent Scottish-born scientist inventor
      -- engineer and innovator who is credited with inventing the first
      -- practical telephone -
      -- https:--en.wikipedia.org/wiki/Alexander_Graham_Bell
    , 'b' :| "ell"

      -- Homi J Bhabha - was an Indian nuclear physicist founding director and
      -- professor of physics at the Tata Institute of Fundamental Research.
      -- Colloquially known as , 'f' :| "ather of Indian nuclear programme"-
      -- https:--en.wikipedia.org/wiki/Homi_J._Bhabha
    , 'b' :| "habha"

      -- Bhaskara II - Ancient Indian mathematician-astronomer whose work on
      -- calculus predates Newton and Leibniz by over half a millennium -
      -- https:--en.wikipedia.org/wiki/Bh%C4%81skara_II#Calculus
    , 'b' :| "haskara"

      -- Elizabeth Blackwell - American doctor and first American woman to
      -- receive a medical degree -
      -- https:--en.wikipedia.org/wiki/Elizabeth_Blackwell
    , 'b' :| "lackwell"

      -- Niels Bohr is the father of quantum theory.
      -- https:--en.wikipedia.org/wiki/Niels_Bohr.
    , 'b' :| "ohr"

      -- Kathleen Booth she's credited with writing the first assembly
      -- language. https:--en.wikipedia.org/wiki/Kathleen_Booth
    , 'b' :| "ooth"

      -- Anita Borg - Anita Borg was the founding director of the Institute for
      -- Women and Technology (IWT). https:--en.wikipedia.org/wiki/Anita_Borg
    , 'b' :| "org"

      -- Satyendra Nath Bose - He provided the foundation for Bose–Einstein
      -- statistics and the theory of the Bose–Einstein condensate. -
      -- https:--en.wikipedia.org/wiki/Satyendra_Nath_Bose
    , 'b' :| "ose"

      -- Evelyn Boyd Granville - She was one of the first African-American
      -- woman to receive a Ph.D. in mathematics; she earned it in 1949 from
      -- Yale University. https:--en.wikipedia.org/wiki/Evelyn_Boyd_Granville
    , 'b' :| "oyd"

      -- Brahmagupta - Ancient Indian mathematician during 598-670 CE who gave
      -- rules to compute with zero -
      -- https:--en.wikipedia.org/wiki/Brahmagupta#Zero
    , 'b' :| "rahmagupta"

      -- Walter Houser Brattain co-invented the transistor -
      -- https:--en.wikipedia.org/wiki/Walter_Houser_Brattain
    , 'b' :| "rattain"

      -- Emmett Brown invented time travel.
      -- https:--en.wikipedia.org/wiki/Emmett_Brown (thanks Brian Goff)
    , 'b' :| "rown"

      -- Rachel Carson - American marine biologist and conservationist her book
      -- Silent Spring and other writings are credited with advancing the
      -- global environmental movement.
      -- https:--en.wikipedia.org/wiki/Rachel_Carson
    , 'c' :| "arson"

      -- Subrahmanyan Chandrasekhar - Astrophysicist known for his mathematical
      -- theory on different stages and evolution in structures of the stars.
      -- He has won nobel prize for physics -
      -- https:--en.wikipedia.org/wiki/Subrahmanyan_Chandrasekhar
    , 'c' :| "handrasekhar"

      -- Claude Shannon - The father of information theory and founder of
      -- digital circuit design theory.
      -- (https:--en.wikipedia.org/wiki/Claude_Shannon)
    , 's' :| "hannon"

      -- Joan Clarke - Bletchley Park code breaker during the Second World War
      -- who pioneered techniques that remained top secret for decades. Also an
      -- accomplished numismatist https:--en.wikipedia.org/wiki/Joan_Clarke
    , 'c' :| "larke"

      -- Jane Colden - American botanist widely considered the first female
      -- American botanist - https:--en.wikipedia.org/wiki/Jane_Colden
    , 'c' :| "olden"

      -- Gerty Theresa Cori - American biochemist who became the third
      -- woman—and first American woman—to win a Nobel Prize in science and the
      -- first woman to be awarded the Nobel Prize in Physiology or Medicine.
      -- Cori was born in Prague. https:--en.wikipedia.org/wiki/Gerty_Cori
    , 'c' :| "ori"

      -- Seymour Roger Cray was an American electrical engineer and
      -- supercomputer architect who designed a series of computers that were
      -- the fastest in the world for decades.
      -- https:--en.wikipedia.org/wiki/Seymour_Cray
    , 'c' :| "ray"

      -- This entry reflects a husband and wife team who worked together: Joan
      -- Curran was a Welsh scientist who developed radar and invented chaff a
      -- radar countermeasure. https:--en.wikipedia.org/wiki/Joan_Curran Samuel
      -- Curran was an Irish physicist who worked alongside his wife during
      -- WWII and invented the proximity fuse.
      -- https:--en.wikipedia.org/wiki/Samuel_Curran
    , 'c' :| "urran"

      -- Marie Curie discovered radioactivity.
      -- https:--en.wikipedia.org/wiki/Marie_Curie.
    , 'c' :| "urie"

      -- Charles Darwin established the principles of natural evolution.
      -- https:--en.wikipedia.org/wiki/Charles_Darwin.
    , 'd' :| "arwin"

      -- Leonardo Da Vinci invented too many things to list here.
      -- https:--en.wikipedia.org/wiki/Leonardo_da_Vinci.
    , 'd' :| "avinci"

      -- Edsger Wybe Dijkstra was a Dutch computer scientist and mathematical
      -- scientist. https:--en.wikipedia.org/wiki/Edsger_W._Dijkstra.
    , 'd' :| "ijkstra"

      -- Donna Dubinsky - played an integral role in the development of
      -- personal digital assistants (PDAs) serving as CEO of Palm Inc. and
      -- co-founding Handspring. https:--en.wikipedia.org/wiki/Donna_Dubinsky
    , 'd' :| "ubinsky"

      -- Annie Easley - She was a leading member of the team which developed
      -- software for the Centaur rocket stage and one of the first
      -- African-Americans in her field.
      -- https:--en.wikipedia.org/wiki/Annie_Easley
    , 'e' :| "asley"

      -- Thomas Alva Edison prolific inventor
      -- https:--en.wikipedia.org/wiki/Thomas_Edison
    , 'e' :| "dison"
        
      -- Albert Einstein invented the general theory of relativity.
      -- https:--en.wikipedia.org/wiki/Albert_Einstein
    , 'e' :| "instein"

      -- Gertrude Elion - American biochemist pharmacologist and the 1988
      -- recipient of the Nobel Prize in Medicine -
      -- https:--en.wikipedia.org/wiki/Gertrude_Elion
    , 'e' :| "lion"

      -- Douglas Engelbart gave the mother of all demos:
      -- https:--en.wikipedia.org/wiki/Douglas_Engelbart
    , 'e' :| "ngelbart"

      -- Euclid invented geometry. https:--en.wikipedia.org/wiki/Euclid
    , 'e' :| "uclid"

      -- Leonhard Euler invented large parts of modern mathematics.
      -- https:--de.wikipedia.org/wiki/Leonhard_Euler
    , 'e' :| "uler"

      -- Pierre de Fermat pioneered several aspects of modern mathematics.
      -- https:--en.wikipedia.org/wiki/Pierre_de_Fermat
    , 'f' :| "ermat"

      -- Enrico Fermi invented the first nuclear reactor.
      -- https:--en.wikipedia.org/wiki/Enrico_Fermi.
    , 'f' :| "ermi"

      -- Richard Feynman was a key contributor to quantum mechanics and
      -- particle physics. https:--en.wikipedia.org/wiki/Richard_Feynman
    , 'f' :| "eynman"

      -- Benjamin Franklin is famous for his experiments in electricity and the
      -- invention of the lightning rod.
    , 'f' :| "ranklin"

      -- Galileo was a founding father of modern astronomy and faced politics
      -- and obscurantism to establish scientific truth.
      -- https:--en.wikipedia.org/wiki/Galileo_Galilei
    , 'g' :| "alileo"

      -- William Henry , 'B' :| "ill' ' :| "Gates III is an American business magnate
      -- philanthropist investor computer programmer and inventor.
      -- https:--en.wikipedia.org/wiki/Bill_Gates
    , 'g' :| "ates"

      -- Adele Goldberg was one of the designers and developers of the
      -- Smalltalk language.
      -- https:--en.wikipedia.org/wiki/Adele_Goldberg_(computer_scientist)
    , 'g' :| "oldberg"

      -- Adele Goldstine born Adele Katz wrote the complete technical
      -- description for the first electronic digital computer ENIAC.
      -- https:--en.wikipedia.org/wiki/Adele_Goldstine
    , 'g' :| "oldstine"

      -- Shafi Goldwasser is a computer scientist known for creating
      -- theoretical foundations of modern cryptography. Winner of 2012 ACM
      -- Turing Award. https:--en.wikipedia.org/wiki/Shafi_Goldwasser
    , 'g' :| "oldwasser"

      -- James Golick all around gangster.
    , 'g' :| "olick"

      -- Jane Goodall - British primatologist ethologist and anthropologist who
      -- is considered to be the world's foremost expert on chimpanzees -
      -- https:--en.wikipedia.org/wiki/Jane_Goodall
    , 'g' :| "oodall"

      -- Lois Haibt - American computer scientist part of the team at IBM that
      -- developed FORTRAN - https:--en.wikipedia.org/wiki/Lois_Haibt
    , 'h' :| "aibt"

      -- Margaret Hamilton - Director of the Software Engineering Division of
      -- the MIT Instrumentation Laboratory which developed on-board flight
      -- software for the Apollo space program.
      -- https:--en.wikipedia.org/wiki/Margaret_Hamilton_(scientist)
    , 'h' :| "amilton"

      -- Stephen Hawking pioneered the field of cosmology by combining general
      -- relativity and quantum mechanics.
      -- https:--en.wikipedia.org/wiki/Stephen_Hawking
    , 'h' :| "awking"

      -- Werner Heisenberg was a founding father of quantum mechanics.
      -- https:--en.wikipedia.org/wiki/Werner_Heisenberg
    , 'h' :| "eisenberg"

      -- Jaroslav Heyrovský was the inventor of the polarographic method father
      -- of the electroanalytical method and recipient of the Nobel Prize in
      -- 1959. His main field of work was polarography.
      -- https:--en.wikipedia.org/wiki/Jaroslav_Heyrovsk%C3%BD
    , 'h' :| "eyrovsky"

      -- Dorothy Hodgkin was a British biochemist credited with the development
      -- of protein crystallography. She was awarded the Nobel Prize in
      -- Chemistry in 1964. https:--en.wikipedia.org/wiki/Dorothy_Hodgkin
    , 'h' :| "odgkin"

      -- Erna Schneider Hoover revolutionized modern communication by inventing
      -- a computerized telephone switching method.
      -- https:--en.wikipedia.org/wiki/Erna_Schneider_Hoover
    , 'h' :| "oover"

      -- Grace Hopper developed the first compiler for a computer programming
      -- language and is credited with popularizing the term , 'd' :| "ebugging' ' :| "for
      -- fixing computer glitches. https:--en.wikipedia.org/wiki/Grace_Hopper
    , 'h' :| "opper"

      -- Frances Hugle she was an American scientist engineer and inventor who
      -- contributed to the understanding of semiconductors integrated
      -- circuitry and the unique electrical principles of microscopic
      -- materials. https:--en.wikipedia.org/wiki/Frances_Hugle
    , 'h' :| "ugle"

      -- Hypatia - Greek Alexandrine Neoplatonist philosopher in Egypt who was
      -- one of the earliest mothers of mathematics -
      -- https:--en.wikipedia.org/wiki/Hypatia
    , 'h' :| "ypatia"

      -- Yeong-Sil Jang was a Korean scientist and astronomer during the Joseon
      -- Dynasty; he invented the first metal printing press and water gauge.
      -- https:--en.wikipedia.org/wiki/Jang_Yeong-sil
    , 'j' :| "ang"

      -- Betty Jennings - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Jean_Bartik
    , 'j' :| "ennings"

      -- Mary Lou Jepsen was the founder and chief technology officer of One
      -- Laptop Per Child (OLPC) and the founder of Pixel Qi.
      -- https:--en.wikipedia.org/wiki/Mary_Lou_Jepsen
    , 'j' :| "epsen"

      -- Irène Joliot-Curie - French scientist who was awarded the Nobel Prize
      -- for Chemistry in 1935. Daughter of Marie and Pierre Curie.
      -- https:--en.wikipedia.org/wiki/Ir%C3%A8ne_Joliot-Curie
    , 'j' :| "oliot"

      -- Karen Spärck Jones came up with the concept of inverse document
      -- frequency which is used in most search engines today.
      -- https:--en.wikipedia.org/wiki/Karen_Sp%C3%A4rck_Jones
    , 'j' :| "ones"

      -- A. P. J. Abdul Kalam - is an Indian scientist aka Missile Man of India
      -- for his work on the development of ballistic missile and launch
      -- vehicle technology -
      -- https:--en.wikipedia.org/wiki/A._P._J._Abdul_Kalam
    , 'k' :| "alam"

      -- Susan Kare created the icons and many of the interface elements for
      -- the original Apple Macintosh in the 1980s and was an original employee
      -- of NeXT working as the Creative Director.
      -- https:--en.wikipedia.org/wiki/Susan_Kare
    , 'k' :| "are"

      -- Mary Kenneth Keller Sister Mary Kenneth Keller became the first
      -- American woman to earn a PhD in Computer Science in 1965.
      -- https:--en.wikipedia.org/wiki/Mary_Kenneth_Keller
    , 'k' :| "eller"

      -- Har Gobind Khorana - Indian-American biochemist who shared the 1968
      -- Nobel Prize for Physiology -
      -- https:--en.wikipedia.org/wiki/Har_Gobind_Khorana
    , 'k' :| "horana"

      -- Jack Kilby invented silicone integrated circuits and gave Silicon
      -- Valley its name. - https:--en.wikipedia.org/wiki/Jack_Kilby
    , 'k' :| "ilby"

      -- Maria Kirch - German astronomer and first woman to discover a comet -
      -- https:--en.wikipedia.org/wiki/Maria_Margarethe_Kirch
    , 'k' :| "irch"

      -- Donald Knuth - American computer scientist author of , 'T' :| "he Art of
      -- Computer Programming' ' :| "and creator of the TeX typesetting system.
      -- https:--en.wikipedia.org/wiki/Donald_Knuth
    , 'k' :| "nuth"

      -- Sophie Kowalevski - Russian mathematician responsible for important
      -- original contributions to analysis differential equations and
      -- mechanics - https:--en.wikipedia.org/wiki/Sofia_Kovalevskaya
    , 'k' :| "owalevski"

      -- Marie-Jeanne de Lalande - French astronomer mathematician and
      -- cataloguer of stars -
      -- https:--en.wikipedia.org/wiki/Marie-Jeanne_de_Lalande
    , 'l' :| "alande"

      -- Hedy Lamarr - Actress and inventor. The principles of her work are now
      -- incorporated into modern Wi-Fi CDMA and Bluetooth technology.
      -- https:--en.wikipedia.org/wiki/Hedy_Lamarr
    , 'l' :| "amarr"

      -- Leslie B. Lamport - American computer scientist. Lamport is best known
      -- for his seminal work in distributed systems and was the winner of the
      -- 2013 Turing Award. https:--en.wikipedia.org/wiki/Leslie_Lamport
    , 'l' :| "amport"

      -- Mary Leakey - British paleoanthropologist who discovered the first
      -- fossilized Proconsul skull - https:--en.wikipedia.org/wiki/Mary_Leakey
    , 'l' :| "eakey"

      -- Henrietta Swan Leavitt - she was an American astronomer who discovered
      -- the relation between the luminosity and the period of Cepheid variable
      -- stars. https:--en.wikipedia.org/wiki/Henrietta_Swan_Leavitt
    , 'l' :| "eavitt"

      -- Daniel Lewin - Mathematician Akamai co-founder soldier 9/11 victim--
      -- Developed optimization techniques for routing traffic on the internet.
      -- Died attempting to stop the 9-11 hijackers.
      -- https:--en.wikipedia.org/wiki/Daniel_Lewin
    , 'l' :| "ewin"

      -- Ruth Lichterman - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Ruth_Teitelbaum
    , 'l' :| "ichterman"

      -- Barbara Liskov - co-developed the Liskov substitution principle.
      -- Liskov was also the winner of the Turing Prize in 2008. -
      -- https:--en.wikipedia.org/wiki/Barbara_Liskov
    , 'l' :| "iskov"

      -- Ada Lovelace invented the first algorithm.
      -- https:--en.wikipedia.org/wiki/Ada_Lovelace (thanks James Turnbull)
    , 'l' :| "ovelace"

      -- Auguste and Louis Lumière - the first filmmakers in history -
      -- https:--en.wikipedia.org/wiki/Auguste_and_Louis_Lumi%C3%A8re
    , 'l' :| "umiere"

      -- Mahavira - Ancient Indian mathematician during 9th century AD who
      -- discovered basic algebraic identities -
      -- https:--en.wikipedia.org/wiki/Mah%C4%81v%C4%ABra_(mathematician)
    , 'm' :| "ahavira"

      -- Maria Mayer - American theoretical physicist and Nobel laureate in
      -- Physics for proposing the nuclear shell model of the atomic nucleus -
      -- https:--en.wikipedia.org/wiki/Maria_Mayer
    , 'm' :| "ayer"

      -- John McCarthy invented LISP:
      -- https:--en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)
    , 'm' :| "ccarthy"

      -- Barbara McClintock - a distinguished American cytogeneticist 1983
      -- Nobel Laureate in Physiology or Medicine for discovering transposons.
      -- https:--en.wikipedia.org/wiki/Barbara_McClintock
    , 'm' :| "cclintock"

      -- Malcolm McLean invented the modern shipping container:
      -- https:--en.wikipedia.org/wiki/Malcom_McLean
    , 'm' :| "clean"

      -- Kay McNulty - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Kathleen_Antonelli
    , 'm' :| "cnulty"

      -- Lise Meitner - Austrian/Swedish physicist who was involved in the
      -- discovery of nuclear fission. The element meitnerium is named after
      -- her - https:--en.wikipedia.org/wiki/Lise_Meitner
    , 'm' :| "eitner"

      -- Carla Meninsky was the game designer and programmer for Atari 2600
      -- games Dodge 'Em and Warlords.
      -- https:--en.wikipedia.org/wiki/Carla_Meninsky
    , 'm' :| "eninsky"

      -- Johanna Mestorf - German prehistoric archaeologist and first female
      -- museum director in Germany -
      -- https:--en.wikipedia.org/wiki/Johanna_Mestorf
    , 'm' :| "estorf"

      -- Marvin Minsky - Pioneer in Artificial Intelligence co-founder of the
      -- MIT's AI Lab won the Turing Award in 1969.
      -- https:--en.wikipedia.org/wiki/Marvin_Minsky
    , 'm' :| "insky"

      -- Maryam Mirzakhani - an Iranian mathematician and the first woman to
      -- win the Fields Medal. https:--en.wikipedia.org/wiki/Maryam_Mirzakhani
    , 'm' :| "irzakhani"

      -- Samuel Morse - contributed to the invention of a single-wire telegraph
      -- system based on European telegraphs and was a co-developer of the
      -- Morse code - https:--en.wikipedia.org/wiki/Samuel_Morse
    , 'm' :| "orse"

      -- Ian Murdock - founder of the Debian project -
      -- https:--en.wikipedia.org/wiki/Ian_Murdock
    , 'm' :| "urdock"

      -- Isaac Newton invented classic mechanics and modern optics.
      -- https:--en.wikipedia.org/wiki/Isaac_Newton
    , 'n' :| "ewton"

      -- Florence Nightingale more prominently known as a nurse was also the
      -- first female member of the Royal Statistical Society and a pioneer in
      -- statistical graphics
      -- https:--en.wikipedia.org/wiki/Florence_Nightingale#Statistics_and_sanitary_reform
    , 'n' :| "ightingale"

      -- Alfred Nobel - a Swedish chemist engineer innovator and armaments
      -- manufacturer (inventor of dynamite) -
      -- https:--en.wikipedia.org/wiki/Alfred_Nobel
    , 'n' :| "obel"

      -- Emmy Noether German mathematician. Noether's Theorem is named after
      -- her. https:--en.wikipedia.org/wiki/Emmy_Noether
    , 'n' :| "oether"

      -- Poppy Northcutt. Poppy Northcutt was the first woman to work as part
      -- of NASA’s Mission Control.
      -- http:--www.businessinsider.com/poppy-northcutt-helped-apollo-astronauts-2014-12?op=1
    , 'n' :| "orthcutt"

      -- Robert Noyce invented silicone integrated circuits and gave Silicon
      -- Valley its name. - https:--en.wikipedia.org/wiki/Robert_Noyce
    , 'n' :| "oyce"

      -- Panini - Ancient Indian linguist and grammarian from 4th century CE
      -- who worked on the world's first formal system -
      -- https:--en.wikipedia.org/wiki/P%C4%81%E1%B9%87ini#Comparison_with_modern_formal_systems
    , 'p' :| "anini"

      -- Ambroise Pare invented modern surgery.
      -- https:--en.wikipedia.org/wiki/Ambroise_Par%C3%A9
    , 'p' :| "are"

      -- Louis Pasteur discovered vaccination fermentation and pasteurization.
      -- https:--en.wikipedia.org/wiki/Louis_Pasteur.
    , 'p' :| "asteur"

      -- Cecilia Payne-Gaposchkin was an astronomer and astrophysicist who in
      -- 1925 proposed in her Ph.D. thesis an explanation for the composition
      -- of stars in terms of the relative abundances of hydrogen and helium.
      -- https:--en.wikipedia.org/wiki/Cecilia_Payne-Gaposchkin
    , 'p' :| "ayne"

      -- Radia Perlman is a software designer and network engineer and most
      -- famous for her invention of the spanning-tree protocol (STP).
      -- https:--en.wikipedia.org/wiki/Radia_Perlman
    , 'p' :| "erlman"

      -- Rob Pike was a key contributor to Unix Plan 9 the X graphic system
      -- utf-8 and the Go programming language.
      -- https:--en.wikipedia.org/wiki/Rob_Pike
    , 'p' :| "ike"

      -- Henri Poincaré made fundamental contributions in several fields of
      -- mathematics. https:--en.wikipedia.org/wiki/Henri_Poincar%C3%A9
    , 'p' :| "oincare"

      -- Laura Poitras is a director and producer whose work made possible by
      -- open source crypto tools advances the causes of truth and freedom of
      -- information by reporting disclosures by whistleblowers such as Edward
      -- Snowden. https:--en.wikipedia.org/wiki/Laura_Poitras
    , 'p' :| "oitras"

      -- Claudius Ptolemy - a Greco-Egyptian writer of Alexandria known as a
      -- mathematician astronomer geographer astrologer and poet of a single
      -- epigram in the Greek Anthology - https:--en.wikipedia.org/wiki/Ptolemy
    , 'p' :| "tolemy"

      -- C. V. Raman - Indian physicist who won the Nobel Prize in 1930 for
      -- proposing the Raman effect. -
      -- https:--en.wikipedia.org/wiki/C._V._Raman
    , 'r' :| "aman"

      -- Srinivasa Ramanujan - Indian mathematician and autodidact who made
      -- extraordinary contributions to mathematical analysis number theory
      -- infinite series and continued fractions. -
      -- https:--en.wikipedia.org/wiki/Srinivasa_Ramanujan
    , 'r' :| "amanujan"

      -- Sally Kristen Ride was an American physicist and astronaut. She was
      -- the first American woman in space and the youngest American astronaut.
      -- https:--en.wikipedia.org/wiki/Sally_Ride
    , 'r' :| "ide"

      -- Rita Levi-Montalcini - Won Nobel Prize in Physiology or Medicine
      -- jointly with colleague Stanley Cohen for the discovery of nerve growth
      -- factor (https:--en.wikipedia.org/wiki/Rita_Levi-Montalcini)
    , 'm' :| "ontalcini"

      -- Dennis Ritchie - co-creator of UNIX and the C programming language. -
      -- https:--en.wikipedia.org/wiki/Dennis_Ritchie
    , 'r' :| "itchie"

      -- Wilhelm Conrad Röntgen - German physicist who was awarded the first
      -- Nobel Prize in Physics in 1901 for the discovery of X-rays (Röntgen
      -- rays). https:--en.wikipedia.org/wiki/Wilhelm_R%C3%B6ntgen
    , 'r' :| "oentgen"

      -- Rosalind Franklin - British biophysicist and X-ray crystallographer
      -- whose research was critical to the understanding of DNA -
      -- https:--en.wikipedia.org/wiki/Rosalind_Franklin
    , 'r' :| "osalind"

      -- Meghnad Saha - Indian astrophysicist best known for his development of
      -- the Saha equation used to describe chemical and physical conditions in
      -- stars - https:--en.wikipedia.org/wiki/Meghnad_Saha
    , 's' :| "aha"

      -- Jean E. Sammet developed FORMAC the first widely used computer
      -- language for symbolic manipulation of mathematical formulas.
      -- https:--en.wikipedia.org/wiki/Jean_E._Sammet
    , 's' :| "ammet"

      -- Carol Shaw - Originally an Atari employee Carol Shaw is said to be the
      -- first female video game designer.
      -- https:--en.wikipedia.org/wiki/Carol_Shaw_(video_game_designer)
    , 's' :| "haw"

      -- Dame Stephanie , 'S' :| "teve' ' :| "Shirley - Founded a software company in 1962
      -- employing women working from home.
      -- https:--en.wikipedia.org/wiki/Steve_Shirley
    , 's' :| "hirley"

      -- William Shockley co-invented the transistor -
      -- https:--en.wikipedia.org/wiki/William_Shockley
    , 's' :| "hockley"

      -- Françoise Barré-Sinoussi - French virologist and Nobel Prize Laureate
      -- in Physiology or Medicine; her work was fundamental in identifying HIV
      -- as the cause of AIDS.
      -- https:--en.wikipedia.org/wiki/Fran%C3%A7oise_Barr%C3%A9-Sinoussi
    , 's' :| "inoussi"

      -- Betty Snyder - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Betty_Holberton
    , 's' :| "nyder"

      -- Frances Spence - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Frances_Spence
    , 's' :| "pence"

      -- Richard Matthew Stallman - the founder of the Free Software movement
      -- the GNU project the Free Software Foundation and the League for
      -- Programming Freedom. He also invented the concept of copyleft to
      -- protect the ideals of this movement and enshrined this concept in the
      -- widely-used GPL (General Public License) for software.
      -- https:--en.wikiquote.org/wiki/Richard_Stallman
    , 's' :| "tallman"

      -- Michael Stonebraker is a database research pioneer and architect of
      -- Ingres Postgres VoltDB and SciDB. Winner of 2014 ACM Turing Award.
      -- https:--en.wikipedia.org/wiki/Michael_Stonebraker
    , 's' :| "tonebraker"

      -- Janese Swanson (with others) developed the first of the Carmen
      -- Sandiego games. She went on to found Girl Tech.
      -- https:--en.wikipedia.org/wiki/Janese_Swanson
    , 's' :| "wanson"

      -- Aaron Swartz was influential in creating RSS Markdown Creative Commons
      -- Reddit and much of the internet as we know it today. He was devoted to
      -- freedom of information on the web.
      -- https:--en.wikiquote.org/wiki/Aaron_Swartz
    , 's' :| "wartz"

      -- Bertha Swirles was a theoretical physicist who made a number of
      -- contributions to early quantum theory.
      -- https:--en.wikipedia.org/wiki/Bertha_Swirles
    , 's' :| "wirles"

      -- Nikola Tesla invented the AC electric system and every gadget ever
      -- used by a James Bond villain.
      -- https:--en.wikipedia.org/wiki/Nikola_Tesla
    , 't' :| "esla"

      -- Ken Thompson - co-creator of UNIX and the C programming language -
      -- https:--en.wikipedia.org/wiki/Ken_Thompson
    , 't' :| "hompson"

      -- Linus Torvalds invented Linux and Git.
      -- https:--en.wikipedia.org/wiki/Linus_Torvalds
    , 't' :| "orvalds"

      -- Alan Turing was a founding father of computer science.
      -- https:--en.wikipedia.org/wiki/Alan_Turing.
    , 't' :| "uring"

      -- Varahamihira - Ancient Indian mathematician who discovered
      -- trigonometric formulae during 505-587 CE -
      -- https:--en.wikipedia.org/wiki/Var%C4%81hamihira#Contributions
    , 'v' :| "arahamihira"

      -- Sir Mokshagundam Visvesvaraya - is a notable Indian engineer. He is a
      -- recipient of the Indian Republic's highest honour the Bharat Ratna in
      -- 1955. On his birthday 15 September is celebrated as Engineer's Day in
      -- India in his memory - https:--en.wikipedia.org/wiki/Visvesvaraya
    , 'v' :| "isvesvaraya"

      -- Christiane Nüsslein-Volhard - German biologist won Nobel Prize in
      -- Physiology or Medicine in 1995 for research on the genetic control of
      -- embryonic development.
      -- https:--en.wikipedia.org/wiki/Christiane_N%C3%BCsslein-Volhard
    , 'v' :| "olhard"

      -- Marlyn Wescoff - one of the original programmers of the ENIAC.
      -- https:--en.wikipedia.org/wiki/ENIAC -
      -- https:--en.wikipedia.org/wiki/Marlyn_Meltzer
    , 'w' :| "escoff"

      -- Andrew Wiles - Notable British mathematician who proved the enigmatic
      -- Fermat's Last Theorem - https:--en.wikipedia.org/wiki/Andrew_Wiles
    , 'w' :| "iles"

      -- Roberta Williams did pioneering work in graphical adventure games for
      -- personal computers particularly the King's Quest series.
      -- https:--en.wikipedia.org/wiki/Roberta_Williams
    , 'w' :| "illiams"

      -- Sophie Wilson designed the first Acorn Micro-Computer and the
      -- instruction set for ARM processors.
      -- https:--en.wikipedia.org/wiki/Sophie_Wilson
    , 'w' :| "ilson"

      -- Jeannette Wing - co-developed the Liskov substitution principle. -
      -- https:--en.wikipedia.org/wiki/Jeannette_Wing
    , 'w' :| "ing"

      -- Steve Wozniak invented the Apple I and Apple II.
      -- https:--en.wikipedia.org/wiki/Steve_Wozniak
    , 'w' :| "ozniak"
  
      -- The Wright brothers Orville and Wilbur - credited with inventing and
      -- building the world's first successful airplane and making the first
      -- controlled powered and sustained heavier-than-air human flight -
      -- https:--en.wikipedia.org/wiki/Wright_brothers
    , 'w' :| "right"

      -- Rosalyn Sussman Yalow - Rosalyn Sussman Yalow was an American medical
      -- physicist and a co-winner of the 1977 Nobel Prize in Physiology or
      -- Medicine for development of the radioimmunoassay technique.
      -- https:--en.wikipedia.org/wiki/Rosalyn_Sussman_Yalow
    , 'y' :| "alow"

      -- Ada Yonath - an Israeli crystallographer the first woman from the
      -- Middle East to win a Nobel prize in the sciences.
      -- https:--en.wikipedia.org/wiki/Ada_Yonath
    , 'y' :| "onath"
    ]
