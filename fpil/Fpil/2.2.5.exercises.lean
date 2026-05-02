def main : IO Unit := do
  let englishGreeting := IO.println "Hello!"
  IO.println "Bonjour!"
  englishGreeting

/-
main
  =>
do
  let englishGreeting := IO.println "Hello!"
  IO.println "Bonjour!"
  englishGreeting
  =>
englishGreeting is evaluated, the IO action is bound to it but not executed because := rather than <-
  =>
"Bonjour!" is printed
  =>
"Hello!" gets printed since the action value bound to englishGreeting is now executed
-/
