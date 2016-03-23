-- From https://gist.github.com/TheSeamau5/25aede445f2942234588


module Components.SelectionList (..) where

-- Selection List --
-- A selection list is a non-empty list that is aware
-- of the current value selected


type alias SelectionList a =
  { previous : List a
  , current : a
  , next : List a
  }



-- Constructor for a selection list


newSelectionList : a -> List a -> SelectionList a
newSelectionList current next =
  { previous = []
  , current = current
  , next = next
  }



-- Go to the next value in the selection list


forward : SelectionList a -> SelectionList a
forward list =
  case list.next of
    [] ->
      list

    x :: xs ->
      { previous = list.current :: list.previous
      , current = x
      , next = xs
      }



-- Go to the previous value in the selection list


back : SelectionList a -> SelectionList a
back list =
  case list.previous of
    [] ->
      list

    x :: xs ->
      { previous = xs
      , current = x
      , next = list.current :: list.next
      }



-- Update current value in selection list


updateCurrent : (a -> a) -> SelectionList a -> SelectionList a
updateCurrent update list =
  { list | current = update list.current }
