::  arvo.hoon patch for %myco vane integration
::
::  This file documents the changes needed to arvo.hoon to support
::  the %myco vane. Per the README, this is the %m %link ++grow arm
::  that needs to be added around line 1748 of arvo.hoon.
::
::  The %myco vane is registered under the letter %m in the vane
::  routing table (similar to how %j is Jael, %g is Gall, etc.)
::
::  ==== ADD TO ARVO.HOON VANE TABLE ====
::
::  In the ++grow arm of the vane routing core, add:
::
::    %m
::      ::  %myco: mycelial network vane
::      ::  Manages virtual arvo instances for blockchain layers
::      %link
::        =/  =type  -:!>(*link:myco)
::        =/  =vase  !>(link:myco)
::        [type vase]
::
::  ==== ADD TO ARVO.HOON VANE DISPATCH ====
::
::  In the ++call arm of the vane routing core, add a case for %m:
::
::    %m
::      =/  myco-core  (myco our)
::      =/  =myco-gate  (myco-core now eny rof)
::      (call:myco-gate duct dud wrapped-task)
::
::  ==== NOTES ====
::
::  The %m letter is chosen because:
::  - %a = Ames, %b = Behn, %c = Clay, %d = Dill
::  - %e = Eyre, %g = Gall, %i = Iris, %j = Jael, %k = Khan
::  - %m is available and mnemonic for Myco
::
::  The %link sub-identifier indicates that %myco contains the
::  %link blockchain vane as a virtual vane within its virtual
::  arvo instances.
::
~
