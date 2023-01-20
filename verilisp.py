#!/usr/bin/env python3
''' a script for converting verilisp '.vl' files into verilog code using the Common Lisp script 'verilisp.cl'.
'''
import os, sys, re, difflib
import subprocess

usage = '''
    $ python verilisp.py 1.hvl
    compiles verilisp file 1.hvl to verilog 1.v
    $ python verilisp.py
    tests verilisp
'''

__dir__ = os.path.dirname(os.path.abspath(__file__))

VERILISP_CMD = 'clisp -modern ' + os.path.join(__dir__, '__verilisp__.cl')
ONLY_MANGLE_MODE = False
DIFFER = difflib.unified_diff
MANGLER = 'v_'    # must match 'verilog-name-mangle' in verilisp.cl
ANTIMANGLER = 'l_'    # let you use lisp's 'and', etc. as 'l_and'
ENABLE_SECRET_BACKQUOTE_PROGN = True
TEST_ON_ARGLESS = False
NAMES_TO_MANGLE = [
    # special forms
     '@', 'fork', 'release', 'assign', 'deassign', 'task', 'function', '=', 'n=',
    'delay', '#', 'wait', 'if', 'module', 'always', 'initial', 'cat', '.',
    'primitive', 'table', 'for', 'fromto', 'forallbits', 'ref', 'comment',
    'b', 'd', 'h', 'o',

    '<=#', 
    
    # cases
    'case', 'cond', 'casex', 'casez',
    
    # $macros
    'setup', 'hold', 'setuphold', 'period', 'width', 'skew', 'recovery',
    'readmemb', 'readmemh', 'sreadmemb', 'sreadmemh',
    'display', 'displayh', 'displayb', 'displayo',
    'fdisplay', 'fdisplayh', 'fdisplayb', 'fdisplayo',
    'write', 'writeh', 'writeb', 'writeo',
    'fwrite', 'fwriteh', 'fwriteb', 'fwriteo',
    'strobe', 'strobeh', 'strobeb', 'strobeo',
    'fstrobe', 'fstrobeh', 'fstrobeb', 'fstrobeo',
    'monitor', 'monitorh', 'monitorb', 'monitoro',
    'fmonitor', 'fmonitorh', 'fmonitorb', 'fmonitoro',
    'fopen', 'fclose', 'fread',
    'time', 'stime', 'realtime', 'scale', 'printtimescale', 'timeformat',
    'stop', 'finish', 'save', 'incsave', 'restart', 'log', 'nolog', 'key', 'nokey',
    'scope', 'showscopes', 'showvars', 'countdrivers', 'list',
    'monitoron', 'monitoroff', 'dumpon', 'dumpoff', 'dump',
    'dumpfile', 'dumplimit', 'dumpflush', 'dumpvar', 'dumpvars', 'dumpall',
    'reset', 'reset_value', 'reset_count',
    'random', 'getpattern', 'rtoi', 'itor', 'realtobits', 'bitstoreal',
    
    # primitives
    'not', 'or', 'and', 'nand', 'nor', 'xor', 'xnor',
    'buf', 'bufif0', 'bufif1', 'notif0', 'notif1',
    'pmos', 'rpmos', 'nmos', 'rnmos', 'cmos', 'rcmos',
    'tran', 'rtran', 'tranif1', 'rtranif1', 'tranif0', 'rtranif0',
    'pullup', 'pulldown', 'supply0', 'supply1',
    
    # type declarators
    'wire', 'wand', 'wor', 'reg', 'trireg', 'integer', 'parameter',
    'reg=',
    
    # backquote macros
    'include', 'define', 'timescale',
    
    # math operators
    '+', '-', '++', '--', '~&', '&', '?', '&&', '*', '/', '//', '%', '<<', '>>', '>', '<', 
    '>>>', '<<<', '==', '!=', '===', '!==', '^', '^~', '~^', '>=', '!',
    '+=', '-=', '*=', '/=', '^=', '%=', '&=', 'bor=', 'lor=',

    # math operator, not assign operator
    '<=',

    # special math operators
    '%<=', '=<',
]
SPECIAL_NAMES_TO_MANGLE = {
    '|': 'bitwise-or',
    '||': 'logical-or',
    '~|': 'bitwise-nor',
    '|~': 'bitwise-nor',
    '|=': 'bor=',
    '||=': 'lor=',
}

def DEBUG(x):
    print(x)
    return x

def mangle(code):
    ''' replace all occurrences of any token in NAMES_TO_MANGLE with a mangled version.
    '''
    for name in NAMES_TO_MANGLE:
        code = re.sub(
            ('\(%s(?=[\s\(\)])' % (ANTIMANGLER + re.escape(name))),    # ) my poor text editor
            ('(' + name),    # )
            re.sub(
                ('\(%s(?=[\s\(\)])' % re.escape(name)),    # )
                ('(' + MANGLER + name),    # )
                code
            )
        )
    for old, new in SPECIAL_NAMES_TO_MANGLE.items():
        code = re.sub(
            ('\(%s(?=[\s\(\)])' % re.escape(old)),    # )
            ('(' + MANGLER + new),    # )
            code
        )
    code = re.sub(r"([0-9]+)'", r"\g<1>\'", code)
    return code

def backquote_let__main__(s):
    ''' let you comma outside backquote, so you don't need to use eval or defmacro explicitly.
    '''
    if ENABLE_SECRET_BACKQUOTE_PROGN:
        return f'(eval `(let ((*__name__* :__main__)) {s}))'
    else:
        return s

def translate(vl_code, file = None):
    if ONLY_MANGLE_MODE:
        mangled_code = mangle(vl_code)
        if file == None:
            return mangled_code
        else:
            file.write(mangled_code)
            return None
            
    p = subprocess.Popen(VERILISP_CMD, shell=True, stdin=subprocess.PIPE, stdout= subprocess.PIPE if file == None else file, close_fds=True)
    i = p.stdin
    vl_code = f'(add-verilisp-path "{__dir__}/lib/")' + vl_code
    i.write(
        (
        backquote_let__main__(
            mangle(vl_code)
        )
        + '\n(__end__)\n'
        ).encode('utf-8')
    )
    i.flush()
    i.close()
    p.wait()
    if p.returncode != 0:
        raise Exception
    if file == None:
        o = p.stdout
        return o.read().decode('utf-8')
    return None

TEST_DIVIDER = '\n%s\n' % ('=' * 80)    # in files in the tests directory, separates the verilisp code from the equivalent verilog code

def test():
    successes = failures = 0
    dn = os.path.join(__dir__, 'tests')
    for fn in os.listdir(dn):
        try:
            with open(os.path.join(dn, fn), mode='r') as f:
                vl_code, v_code = f.read().split(TEST_DIVIDER)
        except:
            print(f'test {fn} is malformed')
            continue
        test = translate(vl_code)
        if test == v_code:
            print('Success:\t' + fn)
            successes += 1
        else:
            print('Failure:\t%s\n    translate(%r)\n    should equal\n    %r\n    but was\n    %r\n    with difference\n%s' % (fn, vl_code, v_code, test, '\n'.join(DIFFER(v_code.split('\n'), test.split('\n')))))
            failures += 1
        print
    print(f'{successes} successes, {failures} failures')

def interactive_interpret():
    global ENABLE_SECRET_BACKQUOTE_PROGN
    import threading
    i, o = os.popen4(VERILISP_CMD)
    def reader():
        line = o.readline()
        while line:
            sys.stdout.write(line)
            sys.stdout.flush()
            line = o.readline()
    try:
        t = threading.Thread(target=reader)
        #t.setDaemon(True)
        t.start()
        s = raw_input()
        while s != '(__end__)':
            if s.strip() == '(__bp_off__)':
                ENABLE_SECRET_BACKQUOTE_PROGN = False
            elif s.strip() == '(__bp_on__)':
                ENABLE_SECRET_BACKQUOTE_PROGN = True
            else:
                i.write(backquote_progn(mangle(s)) + '\n')
                i.flush()
            s = raw_input()
    except:
        pass
    i.close()


def main(argv):
    ''' either translate all filenames in argv to verilog using __verilisp__.cl,
        or call test if no argv.
    '''
    if '-' in argv:
        interactive_interpret()
    elif argv:
        EXT = '.v'
        DIR = None
        next_is_dir = False
        for arg in argv:

            if arg in ['-h', '--help', '-H']:
                print(usage)
            elif arg == '--mangle':
                global ONLY_MANGLE_MODE
                ONLY_MANGLE_MODE = True
                EXT = '.hvlib'
            elif arg == '-t':
                test()
            elif arg == '--dir':
                next_is_dir = True
            elif next_is_dir:
                DIR = arg
                next_is_dir = False
            elif os.path.isfile(arg):
                filename=os.path.splitext(arg)[0] + EXT
                if DIR != None:
                    filename = DIR + '/' + filename
                try:
                    with open(arg, mode='r') as in_f:
                        with open(filename, 'w') as out_f:
                            translate(in_f.read(), out_f)
                except:
                    os.remove(filename)
                    
    elif TEST_ON_ARGLESS:
        test()
    else:
        # for verilisp script "#!" lines
        translate(sys.stdin.read(), sys.stdout)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
