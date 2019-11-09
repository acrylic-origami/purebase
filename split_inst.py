import sys
import os
import re

q = sys.argv[1]

context_rg = lambda pre, bra: re.compile("^" + pre + r"[^=]*=>\s*([^\n]*?) \(?([A-Za-z0-9]*)\s*([^=\n]*?)\)?\s*" + bra + r"(.*?)(?=^[^\s#-]|\Z)", re.MULTILINE | re.DOTALL)
uncontext_rg = lambda pre, bra: re.compile("^" + pre + r"\s*([^=\n \(]*?) \(?([A-Za-z0-9]*)([^=\n\)]*?)\)?\s*" + bra + r"(.*?)(?=^[^\s#-]|\Z)", re.MULTILINE | re.DOTALL) # assume there's a space between the thing we're making and the type variables # fun fact: the outer instance name can only have a single bracket to the left because of the lack of support for partial type applications (e.g. `Maybe Int`)

inst_grep = lambda pre, bra, s: re.findall(uncontext_rg(pre, bra), s) + re.findall(context_rg(pre, bra), s)

def go(p):
	for d in os.listdir(p):
		if not os.path.isdir('%s/%s' % (p, d)):
			if d.endswith('.hs'):
				with open('%s/%s' % (p, d), 'r') as f:
					s = f.read()
					module = re.search(r"(\n|^)module\s*(.*?)\s*(where|\(|\n|$)", s).groups()[1][2:] # get rid of the `C.` namespace
					
					import_rg = re.compile(r"^import(.*?)(?=^[^\s#-][^m]|\Z)", re.MULTILINE | re.DOTALL) # hack to get around `infix`
					imports = re.search(import_rg, s)
					
					classes = inst_grep('class', 'where', s)
					
					data = inst_grep('data', '=', s)
					newtypes = inst_grep('newtype', '=', s)
					gadts = inst_grep('data', 'where', s)
					
					insts = inst_grep('instance', 'where', s)
					
					rgs = [
						context_rg('data', '='),
						uncontext_rg('data', '='),
						context_rg('data', 'where'),
						uncontext_rg('data', 'where'),
						context_rg('newtype', '='),
						uncontext_rg('newtype', '='),
						context_rg('class', 'where'),
						uncontext_rg('class', 'where'),
						context_rg('instance', 'where'),
						uncontext_rg('instance', 'where'),
					]
					for rg in rgs:
						s = re.sub(rg, '', s)
					
					s = re.sub(r"(\n|^)instance.*(\n|$)", '', s) # remove empty instances
				
				with open('%s/%s' % (p, d), 'w') as f:
					
					next_imports = set(
						[k[0] for k in classes] +
						[k[0] for k in data] +
						[k[0] for k in newtypes] +
						[k[0] for k in gadts]
					)
					
					# print(imports.groups()[0])
					# print(insts)
					# print(next_imports)
					
					for inst in insts:
						next_import = None
						if inst[0] in next_imports:
							next_import = inst[0]
						else:
							next_import = inst[1]
						
						next_imports.add(next_import)
						
						mod_name = 'Inst%s%s%s.hs' % (inst[0], inst[1], re.sub(r"[^A-Za-z0-9]", '', inst[2]))
						with open('%s%s' % (sys.argv[2] + '/' if len(sys.argv) > 2 else q + '/', mod_name), 'w') as g:
							g.write('-- Instance of class %s for %s (%s)\nmodule %s where\nimport' % (inst[0], inst[1], inst[2], mod_name))
							g.write(imports.groups()[0])
							g.write('\nimport %s ( %s(..) )\n' % (module, next_import))
							g.write(inst[3])
					
					next_imports_s = 'import %s ( %s )' % (module, ", ".join('%s(..)' % im for im in next_imports))
					if len(next_imports) > 0:
						s = re.sub(import_rg, r"import\1%s\n\n" % (next_imports_s), s)
					else:
						print(d)
					
					f.write(s)
							
		elif d not in ['.', '..']:
			go('%s/%s' % (p, d))
	
go(q)