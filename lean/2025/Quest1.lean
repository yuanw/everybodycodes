def readFileContents (path : System.FilePath) : IO String := do
    IO.FS.readFile path
    
def main : IO Unit := do
    let content ← readFileContents "data/2025/quest1.txt"
    IO.println s!"File content: {content}"
