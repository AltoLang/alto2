import os

src_dir = './src'

# count up lines for all rust source files in src
count = 0
def count_dir(dir):
    global count
    for f in os.listdir(dir):
        name = os.path.join(dir, f)
        if f.endswith('.rs'):
            with open(name, 'r') as f:
                l = len(f.readlines())
                print(f'{f.name}: {l}')
                count += l

        elif os.path.isdir(name):
            count_dir(name)

count_dir(src_dir)

print('---------------------------------')
print(f'total: {count} lines')