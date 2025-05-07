import enum
import re
import operator as op
import types

tokenizer = (r"""\s*(\#'|,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*\n|[^()\s'"`,;]+)(.*)""")
def tokenize(s):    
    a = []
    lines = s.split("\n")
    for line in lines:
        while line:
            match = re.match(tokenizer, line)
            if match:
                token = match.group(1)
                if token != '':
                    a.append(token)
                line = match.group(2)
            else:
                break
    return a

def extract(tokens):
    L = []
    while tokens[0] != ')':
        L.append(ast(tokens))
    tokens.pop(0)
    return L

def atom(token):
    if   re.match(r"^-?\d+\.\d+([eE][-+]?\d+)?$", token):
        return float(token)        
    elif re.match(r"^-?[0-9]\d*$", token):
        return int(token)        
    else:
        return str(token)

def ast(tokens):
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        return extract(tokens)
    elif ')' == token:
        raise SyntaxError('unexpected )')
    elif token in "#'`,@.":
        tk = token
        token = tokens.pop(0)
        if token == '(': return [tk, extract(tokens)]
        else:            return [tk, token ]
    elif token[0] in '":': 
        return ["'", token.replace('"','')]
    else:        
        return atom(token)

class Bytecode: 
    def __init__(self, opcode, arg=None, nargs=0):
        self.opcode = opcode
        self.arg    = arg
        self.nargs  = nargs

    def __repr__(self):
        if type(self.arg) == str:
            if self.nargs == 0 and not self.opcode.name=="CALL":
                return "{}('{}')".format(self.opcode.name, self.arg)
            else:    
                return "{}('{}',{})".format(self.opcode.name, self.arg, self.nargs)
        else:
            return "{}({})".format(self.opcode.name, self.arg)

    def __call__(self, arg, nargs=0):
        return Bytecode(self.opcode, arg, nargs)

    def __eq__(self, other):
        assert(isinstance(other, Bytecode))
        return self.opcode == other.opcode and self.arg == other.arg
    
class Lambda(object):
    def __init__(self, params, body, env, isMacro = False):
        self.params  = params
        self.body    = body
        self.env     = env 
        self.isMacro = isMacro 

    def __call__(self, *actuals):
        if "&rest" in self.params:
            params = [item for item in self.params if item != "&rest"] 
            x, y = params, *actuals
            if len(params) == 1:         
                actuals_record = {x[0]: y}
            else: 
                actuals_record = {x[i]: y[i] if x[(i+1):] != [] else y[i:] for i in range(len(x))}            
        else:            
            params = self.params 
 
            if not isinstance(params, (list)):
                if isinstance(actuals, (list)):
                    actuals_record = dict(zip(params, actuals)) 
                elif not isinstance(*actuals, (list)):
                    actuals_record = dict(zip(params, actuals))
                elif len(params) == len(*actuals):
                    actuals_record = dict(zip(params, *actuals)) 
                else:
                    raise AssertionError(f"zip({params}," , *actuals, ")")
            else:
                actuals_record = dict(zip(params, actuals))             
        
        body_env = Env(actuals_record, self.env)
        return eval(self.body, body_env)

class AutoNumber(enum.Enum):
    def _generate_next_value_(name, start, count, last_values):
        return count
    
@enum.unique
class Opcode(AutoNumber):
    CONST  = enum.auto()
    SET    = enum.auto()
    SETA   = enum.auto()
    GET    = enum.auto()    
    JMP1   = enum.auto()
    JMP0   = enum.auto()
    JUMP   = enum.auto()    
    CALL   = enum.auto()
    RUN    = enum.auto()
    EVAL   = enum.auto()
    LAMBDA = enum.auto()
    MACRO  = enum.auto()
    SPLIC  = enum.auto()
    LP     = enum.auto()
    RP     = enum.auto()

CONST  = Bytecode(Opcode.CONST)
SET    = Bytecode(Opcode.SET)
SETA   = Bytecode(Opcode.SETA)
GET    = Bytecode(Opcode.GET)
JMP1   = Bytecode(Opcode.JMP1)
JMP0   = Bytecode(Opcode.JMP0)
JUMP   = Bytecode(Opcode.JUMP)
CALL   = Bytecode(Opcode.CALL)
RUN    = Bytecode(Opcode.RUN)
EVAL   = Bytecode(Opcode.EVAL)
LAMBDA = Bytecode(Opcode.LAMBDA)
MACRO  = Bytecode(Opcode.MACRO)
SPLIC  = Bytecode(Opcode.SPLIC) 
LP     = Bytecode(Opcode.LP)
RP     = Bytecode(Opcode.RP) 

def compile(exp):
    match exp:
        case ([]):
                return [CONST([])]
        case int(exp) | float(exp) | tuple(exp): 
                return [CONST(exp)]
        case ("'" | "quote", subexp):
                return [CONST(subexp)]
        case (int(exp0) | float(exp0), *subexp):
                return [CONST(exp)]        
        case str(exp) : 
                return [GET(exp)]
        case ("`", subexp): 
                return compileQ(subexp)
        case (".", list(subexp)):
                argsx = compile(subexp)
                argsx.append(SPLIC(0))
                return argsx 
        case (".", str(subexp)):
                argsx = []
                argsx.append(GET(subexp)) 
                argsx.append(SPLIC(0))
                return argsx 
        case (".", subexp):
                argsx = []
                argsx.append(CONST(subexp))
                return argsx
        case ("#'", ('lambda', paras, body)) :                
                cp = compile(body)
                cd = Lambda(paras, cp, env, False)
                return [CONST(cd)]
        case ("#'", ('macro', paras, body)):
                cp = compile(body)[:-1] 
                cd = Lambda(paras, cp, env, True)
                return [CONST(cd)]
        case ("#'", (subexp)):
                fn = env.lookup(subexp)
                return [CONST(fn)]
        case ("dis", subexp):
                if subexp in env.table:
                    x = env.lookup(subexp)
                    if type(x) == Lambda:
                        print(subexp, "<macro>" if x.isMacro else "<lambda>", x.params) # isMacro=True, dis顯示"<macro>"
                        print(x.body)
                    else:
                        print(exp[1], x)
                else:
                    print(f"Not found: {exp[1]}")
                return []
        case ("if", cond, iftrue):
                cond_code    = compile(cond)
                iftrue_code  = compile(iftrue)
                return (cond_code
                        + [JMP0(len(iftrue_code))]
                        + iftrue_code)
        case ("if", cond, iftrue, iffalse):
                cond_code    = compile(cond)
                iftrue_code  = compile(iftrue)
                iffalse_code = compile(iffalse)
                return (cond_code
                        + [JMP0(len(iftrue_code)+1)]
                        + iftrue_code
                        + [JUMP(len(iffalse_code))]
                        + iffalse_code)       
        case ("cond", *subexp):
                arg_code = []
                stackJP = []
                for e in subexp:                
                    cond, iftrue = e
                    cond_code    = compile(cond)
                    iftrue_code  = compile(iftrue)
                    arg_code += (cond_code
                            + [JMP0(len(iftrue_code)+1)]
                            + iftrue_code
                            + [JUMP(999)]
                            )
                    stackJP.append(len(arg_code)-1)
                end = len(arg_code)
                for i in stackJP:
                    arg_code[i] = JUMP(end - i - 1)
                return arg_code
        case ("while", cond, *subexp):
                arg_code  = []
                stackJP   = []
                cond_code = compile(cond)
                arg_code = cond_code + [JMP0(999)]
                stackJP.append(len(arg_code)-1)
                for e in subexp: 
                    arg_code += compile(e)
                end = len(arg_code)
                arg_code += [JUMP(-end-1)]
                for i in stackJP:
                    arg_code[i] = JMP0(end - i)
                return arg_code
        case ("begin", *subexp):
                return [instr for exp in subexp for instr in compile(exp)]
        case ("lambda", list(params), body):
                cmp = CONST(tuple(compile(body)))
                return [CONST(tuple(params)), cmp, LAMBDA(len(params))]
        case ("macro", list(params), body):
                cmp = CONST(tuple(compile(body)))
                return [CONST(tuple(params)), cmp, MACRO(len(params))]    
        case ("define", str(name), list(subexp)):
                cmp = compile(subexp)            
                return cmp + [SET(name)]
        case ("define", str(name), subexp):      
                cmp = compile(subexp)            
                return cmp + [SET(name)]
        case ("set!", str(name), subexp): 
                cmp = compile(subexp)
                return  cmp + [SETA(name)]
        case ("let*", bindings, *subexp): 
                arg_code = []
                cmp      = []
                for b in bindings: 
                    if len(b) == 2:
                        arg_code += compile(b[1]) + [SET(b[0],1)]
                    else:
                        raise AssertionError(f"let* ({b}) -> len({b}) != 2") 
                for e in subexp:     cmp      += compile(e)
                return arg_code + cmp 
        case ("if"|"define"|"set!"|"lambda"|"macro"|"quote"|"'", *subexp):      
                raise AssertionError(f"{exp}")
        case (('macro', params, exp1), *args): 
                nargs = len(params)
                if "&rest" in params:
                    nargs -= 1
                    arg_code = sum([compile(["'", args[i]] if i<nargs-1 else ["'", args[i:]]) for i in range(nargs)], [])
                else:                    
                    if nargs == len(args):
                        arg_code = sum([compile(["'", args[i]]) for i in range(nargs)], [])
                    else:
                         raise AssertionError("{exp}")
                cmp = compile(['macro', params, exp1])       
                return arg_code + cmp + [RUN('', nargs)]
        case ((str(params), *exp1), *args):
                nargs    = len(args)
                arg_code = sum([compile(arg) for arg in args], [])
                cmp = compile(exp[0])       
                return arg_code + cmp + [RUN('', nargs)]
        case (str(exp0), *args):
                if env.chkMacro(exp0):
                    nargs    = len(args)
                    nargs    = len(exp0[1])
                    if len(args)==1 or len(args) != nargs:
                        arg_code = sum([compile(["'", args[i]] if i<nargs-1 else ["'", args[i:]]) for i in range(nargs)], [])
                    else:
                        arg_code = sum([compile(arg) for arg in args], [])
                    cmp = compile(exp0)       
                    return arg_code + cmp + [RUN('', nargs)]
                else:
                    nargs    = len(args)
                    arg_code = sum([compile(arg) for arg in args], [])

                    found = isinstance(args[-1], list) and '.' in args[-1]
                    if found:
                        return [LP(0), CONST(exp0)] + arg_code + [RP(0), EVAL(0)]
                    else:
                        return arg_code +[CALL(exp0,nargs)]
        case _:
              raise NotImplementedError(exp)

def compileQ(exp):
    match exp:
        case (",", *subexp):
                return arg_code
        case list(exp):   
                arg_code = []
                for arg in exp:
                    match arg:
                        case (",", list(subexp)):
                                arg_code.append(compile(subexp))
                        case (",", str(subexp)):
                                arg_code.append(GET(subexp))   
                        case (",@", list(subexp)):
                                arg_code.append(compile(subexp))
                                arg_code.append(SPLIC(0))
                        case (",@", str(subexp)): 
                                arg_code.append(GET(subexp)) 
                                arg_code.append(SPLIC(0))
                        case (",@"|",", int(subexp)|float(subexp)):
                                arg_code.append(CONST(subexp))
                        case list(arg):
                                arg_code.append(compileQ(arg))                   
                        case str(arg) | int(arg) | float(arg):
                                arg_code.append(CONST(arg))

                arg_exp = []
                for arg in arg_code:
                    if isinstance(arg, list):
                        for x in arg:
                            arg_exp.append(x)
                    else:
                        arg_exp.append(arg)
                return [LP(0)] + arg_exp + [RP(0)] 
        case _:
            return [CONST(exp)]

def callcc(proc):
    ball = RuntimeWarning("Sorry, can't continue this continuation any longer.")

    def throw(retval):
        ball.retval = retval
        raise ball

    try:
        return proc(throw)
    except RuntimeWarning as w:
        if w is ball:
            return ball.retval
        else:
            raise w

class Symbol(str):
    pass

def to_string(x):
    if   x is True:
         return "#t"
    elif x is False:
         return "#f"
    elif isinstance(x, Symbol):
         return x
    elif isinstance(x, str):
         return '%s' % bytes(x, "utf-8").decode("unicode_escape").replace('"', r'\"')
    elif isinstance(x, list):
         return '(' + ' '.join(map(to_string, x)) + ')'
    elif isinstance(x, complex):
         return str(x).replace('j', 'i')
    else:
         return str(x)
  
class Env(object):
    def __init__(self, table={}, parent=None):
        self.table  = table
        self.parent = parent
    def __repr__(self):
        return "\n<Env.{}>".format(self.table)

    def define(self, name, value):
        self.table[name] = value

    def assign(self, name, value):
        ret = self.resolve(name)
        if ret is None:
            self.table[name] = value
        else:
            ret.table[name] = value

    def lookup(self, name):
        ret = self.resolve(name)
        if ret is None:
            print(f"not found: {name}")
            return None
        else:
            return ret.table[name]    

    def chkMacro(self, name):
        ret = self.resolve(name)
        if ret is None:
            return False
        else:
            body = ret.table[name]
            if type(body) == Lambda:
                return ret.table[name].isMacro
            else:
                return False

    def resolve(self, name):
        if name in self.table:  return self
        if self.parent is None: 
            return None
        return self.parent.resolve(name)

def setf(x, y, v):
    current = x
    for index in y[:-1]:
        current = current[index]
    current[y[-1]] = v

env = Env()        

env.table.update({
    '+':          op.add, 
    '-':          op.sub, 
    '*':          op.mul, 
    '/':          op.truediv, 
    '>':          op.gt,  
    '<':          op.lt,  
    '>=':         op.ge, 
    '<=':         op.le, 
    '=':          op.eq,
    'equal?':     op.eq,     
    'eq?':        op.is_,   
    'not':        op.not_,
    'append':     op.add,    
    'abs':        abs,
    'length':     len,         
    'max':        max,
    'min':        min,       
    'procedure?': callable,
    'call/cc':    callcc,
    '#t':         True,
    '#f':         [],
    "str+":       lambda x,y: x+y,
    'int':        lambda x:   int(x),
    'type':       lambda x:   type(x),
    'map':        lambda proc,*l: list(map(proc, *l)),
    'apply':      lambda proc,l: proc(*l) if isinstance(proc, types.FunctionType) else proc(l),
    'car':        lambda x:   x[0],
    'cdr':        lambda x:   x[1:], 
    'cons':       lambda x,y: [x] + y,
    'null?':      lambda x:   x == [], 
    'list':       lambda *x:  list(x),
    'zip':        lambda *x:  [list(t) for t in zip(*x)],
    'list?':      lambda x:   isinstance(x, list),     
    'atom?':      lambda x:   not isinstance(x, list),         
    'number?':    lambda x:   isinstance(x, (int, float)), 
    'symbol?':    lambda x:   isinstance(x, str),
    'echo':       lambda *x:  print(*x, sep='', end=''),
    'print':      lambda *x:  print(*x),
    'printf':     lambda x:   print(to_string(x)), 
    'input':      input,
    'evalsrc':    lambda x:   eval(compile(ast(tokenize(x))), env),  
    'eval':       lambda x,e: eval(compile(x), e), 
    'env':        env,
    'env+':       lambda x,y: Env({x:y}, env),
    'fib':        Lambda(('x',), (
                     GET('x'), CONST(3), CALL('<', 2), 
                     JMP1(10),
                     GET('x'), CONST(1), CALL('-', 2), CALL('fib', 1), 
                     GET('x'), CONST(2), CALL('-', 2), CALL('fib', 1), 
                     CALL('+', 2), 
                     JUMP(1),
                     CONST(1),   
                    ), env, False),
    'setf':       setf,
})


def eval(code, env):
    pc = 0
    stack  = []
    stackQ = []

    while pc < len(code):
        ins = code[pc]
        op  = ins.opcode
        pc += 1
        if   op == Opcode.CONST:
            stack.append(ins.arg)
        elif op == Opcode.SET:
            val = stack.pop(-1)            
            env.define(ins.arg, val)
        elif op == Opcode.SETA:
            val = stack.pop(-1) 
            env.assign(ins.arg, val)
        elif op == Opcode.GET:
            val = env.lookup(ins.arg)
            stack.append(val)
        elif op == Opcode.JMP1:
            cond = stack.pop(-1)
            if cond != False and cond !=[]:
                pc += ins.arg 
        elif op == Opcode.JMP0:            
            cond = stack.pop(-1)
            if cond == False or cond == [] :
                pc += ins.arg
        elif op == Opcode.JUMP:
            pc += ins.arg
        elif op == Opcode.LAMBDA or op == Opcode.MACRO:
            nargs     = ins.arg
            body_code = stack.pop(-1)
            params    = stack.pop(-1)
            assert len(params) == nargs
            isMacro = True if op == Opcode.MACRO else False
            stack.append(Lambda(params, body_code, env, isMacro))
        elif op == Opcode.SPLIC:
            args = stack.pop(-1) 
            if isinstance(args, list):                
                for a in args:
                    stack.append(a)
            else:
                stack.append(args)
        elif op == Opcode.LP:
            stackQ.append(len(stack))
        elif op == Opcode.RP and stack != []:
            idx  = stackQ.pop(-1)      
            args = [stack.pop(-1) for i in range(len(stack)-idx)][::-1]
            stack.append(list(args))
        elif op == Opcode.EVAL and stack != []:
            src  = stack.pop(-1)
            fn   = env.lookup(src[0])
            args = src[1:]            
            if type(fn).__name__== 'Lambda': # and fn.isMacro:
                stack.append(fn(args))
            else:
                stack.append(fn(*args))
        elif (op == Opcode.CALL or op == Opcode.RUN):
            if(op == Opcode.RUN):           
                fn = stack.pop(-1)
            else:
                fn = env.lookup(ins.arg)

            args = [stack.pop(-1) for i in range(ins.nargs)][::-1]
 
            if callable(fn):           
                if type(fn).__name__== 'Lambda':
                    if  fn.isMacro:
                        if type(args[0]) == list or type(args[0]) == tuple :   
                            stack.append(eval(compile(fn(*args)), env))
                        else:
                            stack.append(eval(compile(fn(args)), env))
                    else:
                        stack.append(fn(args))
                else:
                    stack.append(fn(*args))
            else:
                raise AssertionError(f"not callable: {fn}")
        else:
            raise AssertionError(f"stack error: ins = {ins} stack = {stack}")
    if stack:
        return stack[-1]

def chkPair(stk, line):
    line = re.sub(r';.*', '',line)

    for char in line:
        if char == '(':
            stk.append(char)
        elif char == ')':
            if len(stk) == 0 or stk[-1] != '(':
                break
            else:
                stk.pop()
    return line

def read(file_name=None):
    stk = []
    lines = []

    with open(file_name, 'rb') as f:
        for inp in f:
            line = inp.decode()
            line = chkPair(stk, line)
            lines.append(line)
            
            if not stk:
                text = ' '.join(lines)
                lines = []

                if text != "\n":
                    try:
                        if re.fullmatch(r"\s*", text) is None:
                            eval(compile(ast(tokenize(text))), env)
                    except:
                        print(f"Error:  '{text}'")

def inputs():
    stk = []
    lines = []
    while True:
        line = input().strip() 
        line = chkPair(stk, line)
        lines.append(line)
        if not stk:
            break
    return ' '.join(lines)

def repl():
    while True:
        print("llisp> ", end="")
        text = inputs()
        val = eval(compile(ast(tokenize(text))), env)
        if val is not None: 
            print(val)

read("init.lsp")

if __name__=='__main__':
    repl()
