import sys

def create_solution(name):
    lower_name = name.lower() 
    title_name = name.title()

    with open(f"data/{lower_name}.txt", "w") as _: pass

    with open("Solution.hs.tmpl", "r") as r:
        hs_contents = r.read().replace("$NAME", title_name)
    with open(f"lib/{title_name}.hs", "w") as w:
        w.write(hs_contents)

    with open("Codyssi.cabal", "r+") as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if "exposed-modules:" in line:
                lines.insert(i + 1, f"    {title_name}\n")
                break
        f.seek(0); f.writelines(lines)

    with open("app/Main.hs", "r+") as f:
        main_contents = f.read()
        main_with_import = main_contents.replace("\n\nmain :: IO ()", f"\nimport {title_name} qualified\n\nmain :: IO ()")
        main_with_solve = main_with_import.replace("\n  ]", f',\n    ("{lower_name}", solve "{lower_name}" {title_name}.solve)\n  ]')
        f.seek(0); f.write(main_with_solve)

if __name__ == "__main__":
    create_solution(sys.argv[1])