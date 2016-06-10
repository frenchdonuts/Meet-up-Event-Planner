module Components.TwoSelectionListNavigator exposing (..)

import Components.SelectionList as SL exposing (..)

-- Data structure to navigate between sequences of pages (flows)
type alias TwoSelectionListNavigator page =
  { current : SL.SelectionList page
  , other : SL.SelectionList page
  }

newNavigator : SL.SelectionList page -> SL.SelectionList page -> TwoSelectionListNavigator page
newNavigator current other =
  { current = current, other = other }

current : TwoSelectionListNavigator page -> page
current navigator = navigator.current.current

toggle : TwoSelectionListNavigator page -> TwoSelectionListNavigator page
toggle navigator =
  { current = navigator.other, other = navigator.current }

forward : TwoSelectionListNavigator page -> TwoSelectionListNavigator page
forward navigator =
  { navigator | current = SL.forward navigator.current }

back : TwoSelectionListNavigator page -> TwoSelectionListNavigator page
back navigator =
  { navigator | current = SL.back navigator.current }

updateCurrent : (page -> page) -> TwoSelectionListNavigator page -> TwoSelectionListNavigator page
updateCurrent update navigator =
  { navigator | current = SL.updateCurrent update navigator.current }
