# Torxakis Changelog

## v0.6.0

Removed hlint warnings from LPE

Added missed tokens from TxsAlex.x in TxsHappy.y (#622)

Updated Hlint to 2.1 (#612)

solved bug: CONSTDEF of const ValExpr fails (#608)

Fixed haddock docs (#606)

Moved to lts 11.1 (#603)

Parametrized Sut/Sim delta times (#600)

- This fixes long standing bug that Sim_deltaTime being ignored (#30)

SortError is removed (#597)

Updated Z3 to 4.6.0 and CVC4 to 20180306-nightly (#596)

Add a macOS installer for `TorXakis`. Thanks to Harco Kuppens. (#591)

All TorXakis parameters now can be configured from config (`.torxakis.yaml`) file (#564)

improved cstrITE based on LPE experience (#581)

merged solve and solvedefs packages (#577)

Merged Lpe (#574)

## v0.5.0

Changed the location of the `text-via-sockets` repository. (#566)

Issue/532 stop sut (#563)

Cache .stack-work folder in AppVeyor (#562)

Implemented checkSMTSolvers (#560)

Improved haddock documentation (#557)

Upgraded to LTS 10.0 (#558)

weeder output used (#556)

hlint issues removed (#554)

Made solve independent of txsdefs(#551)

Fix/java refactor (#550)

Refactoring java code for maintainability (#547)

fixed wrong license reference (#545)

Added Bool back to predefSort (#541)

Disabled GHC optimizations for front (#539)

Equality of behaviour expressions modulo unique id's (#538)

Introduced a valexpr package

Improved output for various SUTs (#531)

Enabled Git-LFS (#529)

Increased sim steps from 120 to 130 (#530)

Added extra parameter to subst (#527)

Test Purpose improvements (#499)

Added CustomersOrders powerpoint (#441)

Removed CallGraph as it is dead code (#524)

Updated MovingArms model (#485)

Removed TODOs and created issues for them (#458)

Updated the README and docs/wiki/README (#438)

Remove the duplicates from the TorXakis command history (#411)

Feat/start txsserver from ui (#394)

report selected Solver (#392)

Issue/stautdef not initialized (#388)

Fixed ReadWriteConflict-HitAll7 test (#390)

Disabled Restricted Axis Purpose Test (#387)

Moved modelling examples' docs into examples folder (#383)

Improved Stautdef code (#382)

Feat/add command history support (#380)

Implemented creating TorXakis snap package (#372)

Fixed showing logs for failed tests (#373)

Reorganized AppVeyor cache (#343)

Fixed CustomersOrders test (#345)

Updated SharedMem docs (#342)

Added stepper tests to explore models (#339)

ITE made smarter (#337)

Enable pedantic build (#333)

Simplification of model (#335)

No line buffering for sqatt (#327)

Added copyright notice to .appveyor.yml (#322)

Using AppVeyor for Windows CI (#321)

Implemented dumping log files when a test fails (#320)

Reverted the stack.yaml file back to its Windows version(#317)

Feat/lpe prepare (#314)

Added ANY to ValExpr (#296)

Removed irrelevant seeds from .txscmd files (#310)

Issue133/cstr function call (#313)

Added copyright test to setup.sh (#311)

Added Test Purpose for Point example (#304)

Added Test Purpose for ReadWriteConflict (#305)

Added CHANGELOG.md (#306)

Feat/bench with real world examples (#302)

Used Test Purposes in Moving Arms example (#301)

Added TestPurpose tests for Adder example (#299)

Changed Dispro12 Right and Wrong to stepper (#292)

Issue133/regexrepr (#283)

All benchmarks run by `stack bench` (#281)

## v0.4.0

Fixed Command line argument variables not working (#277)

Report the number of test objectives when loading model (#273)

Improved syntax of example based on feedback Jan (#274)

Improved model (#272)

Feat/test selection replay (#259)

Implements Test input eagerness with TestPurpose (#271)

Migrated benchmarks to sqatt (#266)

step 1 of #91 (#264)

Typo in config file (#263)

symbolic strings (#260)

improve naming (#257)

Introduce the pending (claimed by me) improvements to sum and prod (#256)

added GEZ constructor to ValExpr (#255)

Generalized restricted movement (#252)

Added STAUTDEF variant of TestPurpose (#249)

Added symbolic sum and product to valexpr (#244)

Fixed wobbly tests (#237)

Replaced purpose models with TestPurposes in LuckyPeople (#231)

Added constructors for Modulo and Divide (#236)

Issue/192 act offer (#233)

Removed dead code (#234)

Made guard with a single condition (#232)

removed dependency on text-show (#230)

Replaced String by Text as a type alias for Name (#226)

Fix/bug 217 (#225)

Change cvc4 version to v1.5 (#224)

Set IncrementChoice as default random value generator (#209)

Update z3 to a recent nightly build (#223)

Missed hlint warnings (#220)

minor improvements (#219)

Added ReadWriteConflict to sqatt (#206)

Added CustomersOrders test to sqatt (#204)

Made Tests in TestBExpr working again (#202)

Torxakis configuration via config file (#198)

Added logo to README.md (#194)

added simple replay functionality using test purposes (#196)

Fixed error popping up in example Dispro10 and menu computation (#190)

Fixed haddock docs location (#193)

Fixed haddock problems (#189)

Fixed mapper example MAdder.txs(#179)

Removed hlint issues in test functions (#174)

Factored out configuration related functionality from the server. (#171)

Feat/document how to add tests (#170)

Migration to SemaphoreCI (#168)

Upgrade to Stack LTS 9.1 (#167)

Fix/hlint (#165)

Fix/pedantic (#164)

Feat/migrate dispatch process tests (#163)

Migrated LuckyPeople and ControlLoop examples (#158)

fixing a lot of pedantic error (-Wall) (#156)

Added logging for squatt tests (#152)

Build develop and hotfixes for every change (#155)

Fixed Copyright notice echoed in bat files (#153)

Cleaned up weeds repordet by weeder (#148)

Issue/lts (#150)

Feat/migrate adder tests (#143)

change ITE from list to single vexpr. (#142)

Migrated StimulusResponse and Echo tests

Fix txcmd file names in example tests (#101)

Added status badges

Fixed build checks (#99)

Fixed Echo simulator example (#97)

Fixed copyright notices (#95)

Auto-deploy Haddock docs to gh-pages (#94)

Allow smt solver selection (#88)

Fixed test purposes

Added Code Climate integration (#82)

Feat/testpurpose (#61)

Ncomp (#59)

improved documentation based on feedback (#54)

Feat/randomization (#53)

--fast flag and wait for 50min (#52)

Doc/txscore (#45)

cstrAnd and cstrOr work on Sets - so random generation can't control order of elements when using ValExprs anymore (#51)

Enabled CI by Travis (#49)

Symbolic elimination of not and and constructors. (#48)

added explicit types to grammar (without attributes) (#46)

improved documentation [Pierre van de Laar]

Fix eval ASF (#43)

Added timestamps to DAT log and some polish (#39)

Split the data structures for parsing and computations (#37)

Testing all examples in DAT (#36)

Removed the stack files that were not used. (#33)

Added explicit synchronization. (#34)

Fix/unswap deltas (#31)


## v0.3.0

Issues/1 simul fixed (#26)

Fixed all dispatchprocess examples (#25)

Updated cvc4 to cvc4-2017-06-13-win32-opt (#23)

Fixed some HLint messages (#22)

Removed DefNo (PR#21)

removed NoId (#17)

Fixed cabal files to show license correctly as BSD3. (#16)

Rename CHANDEF in LuckyPeople example (#15)

Corrected Copy-paste- not modified error [Pierre van de Laar]

Improved handling of messages from SMT Solver (#14)

Added real world example

Updated LuckyPeople example against possible overflow

Changed extensions of log files to '.log'

Added log, smt2 and datr files to be ignored. (#10)

Re-enabled echo'ed benchmark tests. (#9)