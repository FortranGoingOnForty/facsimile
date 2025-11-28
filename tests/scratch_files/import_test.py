# Test file for code actions - should offer "add import" suggestions

# Using datetime without importing it - Pyright should offer to add import
today = datetime.date.today()

# Using Path without importing - should offer to add import
p = Path("/tmp/test.txt")

# Using json without importing - should offer to add import
data = json.loads('{"key": "value"}')

# Using os without importing - should offer to add import
cwd = os.getcwd()
