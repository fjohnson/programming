#!/usr/bin/python

'''
This is an implementation in python of the algorithm described
from http://swtch.com/~rsc/regexp/regexp1.html. Its not complete yet
or rather I forget what is missing that makes it incomplete.
Probably the on the fly DFA conversion.
More over see: http://swtch.com/~rsc/regexp/nfa.c.txt

Date:Feb-May? 2011
Author: Fletcher Johnson
Email: flt.johnson@gmail.com
'''

import sys

def re2post(re):
    #()*|?+
    parens = []
    natoms = 0
    nors = 0
    dst = []

    for renext in re:

        if renext == '(':
            if natoms == 2: 
                dst.append('.')
                natoms = 1

            parens.append((natoms, nors))
            natoms = 0
            nors = 0
    
        elif renext == ')':
            assert parens, 'Parenthesis in regular expression do not match'
            assert natoms, 'Parenthesis must contain a valid regular expression'
            
            if natoms == 2: dst.append('.')
            if nors: dst.append('|'*nors)
            
            natoms,nors = parens.pop()
            natoms+=1
        
        elif renext == '*' or renext == '?' or renext == '+': 
            assert natoms, renext + 'must follow atom'
            dst.append(renext)
            
        elif renext == '|':
            assert natoms, 'atom must preceed |'
            if natoms == 2: dst.append('.')
            natoms = 0
            nors+=1
            
        else:
            if natoms == 2: dst.append('.'); natoms = 1
            dst.append(renext)
            natoms += 1
    
    if natoms == 2: dst.append('.')
    if nors: dst.append('|'*nors)
    return reduce(lambda x,y: x+y, dst, '')

def post2nfa(postre):
    fragstack = []
    class state:
        def __init__(self,c,out=None,out1=None):
            self.c = c
            if not out: self.out = self
            else: self.out = out
            self.out1 = out1
    class frag:
        def __init__(self,start,out=None):
            self.start = start
            if not out: self.out = start
            else: self.out = out

    for c in postre:
        if c == '.':
            f2 = fragstack.pop()
            f1 = fragstack.pop()
            patch(f1.out,f2.start)
            fragstack.append(frag(f1.start,f2.out))
        elif c == '?':
            lastfrag = fragstack.pop()
            zoostate= state(-1,out1=lastfrag.start)
            nextfrag = frag(zoostate,lastfrag.out)
            link(lastfrag.out,zoostate)
            fragstack.append(nextfrag)
        elif c == '*':
            lastfrag = fragstack.pop()
            zomstate = state(-1,out1=lastfrag.start)
            nextfrag = frag(zomstate,zomstate)
            patch(lastfrag.out,zomstate)
            fragstack.append(nextfrag)
        elif c == '+':
            lastfrag = fragstack.pop()
            ormstate = state(-1,out1=lastfrag.start)
            nextfrag = frag(lastfrag.start,ormstate)
            patch(lastfrag.out,ormstate)
            fragstack.append(nextfrag)
        elif c == '|':
            f2 = fragstack.pop()
            f1 = fragstack.pop()
            orstate = state(-1,f1.start,f2.start)
            link(f1.out,f2.out)
            fragstack.append(frag(orstate,f1.out))
        else:
            fragstack.append(frag(state(c)))
    
    if fragstack:
        f1 = fragstack.pop()
        patch(f1.out,state(-2))
        return f1.start
    return state(-2)

def link(state,exit_state):
    while state.out != state: state = state.out
    state.out = exit_state

def patch(state,exit_state):

    while True:
        if state.out == state:
                state.out = exit_state
                break
        temp = state.out
        state.out = exit_state
        state = temp

def epsilonclosure(state,cstates):
    if state.c == -1: #split type state (?+|*)
        epsilonclosure(state.out,cstates)
        epsilonclosure(state.out1,cstates)
    else: cstates.add(state)

def step(startstates,inputc):
    #Step instructions:
    #Compute the closure for each state reachable from the start list
    #but do not add duplicate states. For those steps that do not match
    #the step char, abandon them. For those that do, add their output 
    #to the list.

    eclosure = set()
    for state in startstates:
        epsilonclosure(state,eclosure)

    startstates.clear()
    for state in eclosure:
        if state.c == inputc: startstates.add(state.out)
    
def simulate(machine,input):

    #TODO: Do the epsilon closure first. Then in step do the closure after
    #Stepping on an epsilon closed list. This makes more sense when we switch
    #to using the dfa construct.

    cstate = set([machine])
    for c in input: step(cstate,c)

    #Calculate the closure one last time since it is done only on the
    #start of an input. See step.
    
    finalstates = set()
    for state in cstate:
        epsilonclosure(state,finalstates)
    
    #Check that there is at least one matching state.
    if finalstates: 
        for state in finalstates: 
            if state.c == -2: return True
    return False

result = re2post(sys.argv[1])
print 'Out: %s' % result
machine = post2nfa(result)
print 'Match: %s' % simulate(machine,sys.argv[2])


