open List.MergeSort.Internal

def Test.lengths := [
  10000, 25000, 50000, 75000,
  100000, 200000, 300000, 400000, 500000, 750000,
  1000000, 2000000, 5000000, 7500000,
  10000000
]

def Test.repeats := 2

namespace Test

abbrev Case := List (List Nat)

def Case.ofLength (len : Nat) : IO Test.Case := do
  let l₁ := List.iota len
  let l₂ := List.range len
  let l₃  ← l₂.mapM (fun _ => IO.rand 0 10000000)
  let l₄ := (List.range <| len / 1000).bind fun k =>
    (k * 1000 + 1) :: (k * 1000) :: List.range' (k * 1000 + 2) 998
  return [l₁, l₂, l₃, l₄]

abbrev SortingFn := ∀ {α}, (α → α → Bool) → (List α) → List α

def Case.run (case : Test.Case) (sort : SortingFn) : IO (Nat × Bool) := do
  let mut succs := []
  let mut total := 0
  for _ in [:repeats] do
    let start ← IO.monoMsNow
    for l in case do
      let sorted := sort (· ≤ ·) l
      succs := (sorted.length == l.length) :: succs
    total := (← IO.monoMsNow) - start + total
  let avg := total / (case.length * repeats)
  let succ := succs.all (· == true)
  return (avg, succ)

end Test

def germanMode := true

def main : IO Unit := do
  let mut speedups := []
  let mut succs := []
  for len in Test.lengths do
    let case ← Test.Case.ofLength len
    let (avg₂, succ₂) ← case.run mergeSortTR₂
    let (avg₃, succ₃) ← case.run <| mergeSortTR₃ 32 -- TODO: Make the cutoff variable.
    let speedup := 100 * ((avg₂.toFloat - avg₃.toFloat) / avg₂.toFloat)
    speedups := speedups.concat speedup
    succs := succs ++ [succ₂, succ₃]
    IO.println s!"{len},\"{speedup.toString.take 5}\""

  -- We drop the first two test cases when computing the average, as they don't seem representative.
  let speedups' := speedups.drop 2

  let succ   := succs.all (· == true)
  let avg    := (speedups'.foldl (· + ·) 0) / speedups'.length.toFloat
  let avgStr := if germanMode then avg.toString.replace "." "," else avg.toString

  IO.println s!"Ø,\"{avgStr.take 5}\""
  IO.Process.exit (if succ then 0 else 1)
