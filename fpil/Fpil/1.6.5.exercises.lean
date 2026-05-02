-- Write a function to find the last entry in a list. It should return an Option.
def List.findLast {α : Type} (l : List α) : Option α :=
  match l with
  | [] => Option.none
  | y :: ys =>
    match findLast ys with
      | Option.none => Option.some y
      | Option.some v => Option.some v

-- Write a function that finds the first entry in a list that satisfies a given predicate. Start the definition with def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α := ….
def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => Option.none
  | y :: ys =>
    if predicate y then
      y
    else
      List.findFirst? ys predicate

-- Write a function Prod.switch that switches the two fields in a pair for each other. Start the definition with def Prod.switch {α β : Type} (pair : α × β) : β × α := ….
def Prod.switch {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)

-- Rewrite the PetName example to use a custom datatype and compare it to the version that uses Sum.
inductive PetName : Type where
  | dog : (name : String) → PetName
  | cat : (name : String) → PetName

-- i guess the idea is that this works equivalently with Sum but inl and inr carry little meaning syntactically

def animals : List PetName :=
  [PetName.dog "Spot", PetName.cat "Tiger", PetName.dog "Fifi", PetName.dog "Rex", PetName.cat "Floof"]

def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | PetName.dog _ :: remPets => howManyDogs remPets + 1
  | PetName.cat _ :: remPets => howManyDogs remPets

#eval howManyDogs animals

-- Write a function zip that combines two lists into a list of pairs. The resulting list should be as long as the shortest input list. Start the definition with def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) := ….
def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs with
  | [] => List.nil
  | x :: xs' =>
    match ys with
    | [] => List.nil
    | y :: ys' => (x, y) :: zip xs' ys'

-- Write a polymorphic function take that returns the first nn entries in a list, where nn is a Nat. If the list contains fewer than nn entries, then the resulting list should be the entire input list. #eval take 3 ["bolete", "oyster"] should yield ["bolete", "oyster"], and #eval take 1 ["bolete", "oyster"] should yield ["bolete"].
def take {α : Type} (n : Nat) (xs : List α) : List α :=
  match n with
  | 0 => List.nil
  | Nat.succ n' =>
    match xs with
    | [] => List.nil
    | x :: xs' => x :: take n' xs'

#eval take 3 ["bolete", "oyster"]
#eval take 1 ["bolete", "oyster"]

-- Using the analogy between types and arithmetic, write a function that distributes products over sums. In other words, it should have type α × (β ⊕ γ) → (α × β) ⊕ (α × γ).
def distribute {α β γ : Type} (x : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match x.snd with
  | Sum.inl b => Sum.inl (x.fst, b)
  | Sum.inr g => Sum.inr (x.fst, g)

-- Using the analogy between types and arithmetic, write a function that turns multiplication by two into a sum. In other words, it should have type Bool × α → α ⊕ α.
def mapToSum {α : Type} (x : Bool × α) : α ⊕ α :=
  match x.fst with
  | Bool.true => Sum.inl x.snd
  | Bool.false => Sum.inr x.snd
