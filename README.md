# asana-to-github
Small Haskell tool to import issues into Github from Asana CSV/JSON

# Instructions

1. Make public access tokens at asana (My Profile settings > Apps > Manage developer apps) and github (settings > Personal access tokens)
2. Copy src/Config.template to src/Config.hs and include the tokens in it
3. Update the github and repo name for target repo.

Right now, it reads the source json with asana tasks from a file issues.json in the current directory.

Run the main function from repl or just execute the program from cmd line.
