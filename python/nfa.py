#!/usr/bin/python

'''
This is an implementation in python of the algorithm described
from http://swtch.com/~rsc/regexp/regexp1.html. 
More over see: http://swtch.com/~rsc/regexp/nfa.c.txt

Date:Feb-May? 2011 
Date2: May 24th 2011. Added DFA construction and cleaned up the code.
Author: Fletcher Johnson
Email: flt.johnson@gmail.com
'''

import sys

def re2post(re):
    '''Convert a regular expression into postfix form. The regular
expression can only support the special symbols #()*|?+'''
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
    '''Create a NFA from a postfix regular expression'''

    fragstack = []
    class state:
        def __init__(self,c):
            self.c = c
            self.out = None
            self.out1 = None
            #for patch, if state is a split then join out1 with new state 
            #otherwise join out.

    class frag:
        '''A "fragment" of computation'''

        def __init__(self,start,dangling_states):
            self.start = start
            self.out = dangling_states
            #out is a list of states with dangling arrows.

    for c in postre:
        if c == '.':
            f2 = fragstack.pop()
            f1 = fragstack.pop()
            patch(f1.out,f2.start)
            f1.out = f2.out
            fragstack.append(f1)
        
        elif c == '?':
            f = fragstack.pop()
            zoostate = state(-1)
            zoostate.out = f.start
            f.out.append(zoostate)
            f.start = zoostate
            fragstack.append(f)

        elif c == '*':
            f = fragstack.pop()
            zomstate = state(-1)
            zomstate.out = f.start
            patch(f.out,zomstate)
            f.out = [zomstate]
            f.start = zomstate
            fragstack.append(f)

        elif c == '+':
            f = fragstack.pop()
            ormstate = state(-1)
            ormstate.out = f.start
            patch(f.out,ormstate)
            f.out = [ormstate]
            fragstack.append(f)
            
        elif c == '|':
            f2 = fragstack.pop()
            f1 = fragstack.pop()
            orstate = state(-1)
            orstate.out = f2.start
            orstate.out1 = f1.start
            f1.start = orstate
            f1.out = f1.out + f2.out
            fragstack.append(f1)

        else:
            atom_state = state(c)
            f = frag(atom_state,[atom_state])
            fragstack.append(f)

    if fragstack: 
        f = fragstack.pop()
        patch(f.out, state(-2))
        return f.start
    return state(-2)

def patch(dangling_states, output):
    '''Connect the dangling output of nodes to an actual output'''

    for state in dangling_states:
        if state.c == -1: state.out1 = output
        else: state.out = output;

#A set that maintains the list of visited nodes while computing 
#the epsilon closure.
__visited_state_set = set() 

#A set used to hold the epsilon closure result.
__eclosure_result_set = set()

def epsilonclosure(states):
    ''' Return a new set of states which is the result of performing the
epsilon closure on the input set of states'''

    __eclosure_result_set.clear()
    __visited_state_set.clear()
    map(__epsilonclosure, states)
    return __eclosure_result_set

def __epsilonclosure(state):
    ''' Private epsilon closure worker method '''

    if state.c == -1 and state not in __visited_state_set: #split type state (?+|*)

        #To avoid infinite loops make sure we don't recurse on a node 
        #already visited. The pattern a?* is an example of a scenario 
        #where a circular reference is created between the ? and * node.
        __visited_state_set.add(state) 

        __epsilonclosure(state.out)
        __epsilonclosure(state.out1)
    else: __eclosure_result_set.add(state)

#A set to hold the resulting set of states after transitioning on the input
#symbol (post epsilon closure)
__next_states_result_set = set()

def step(startstates,inputc):
    ''' Return a new set of states, the result of which is derived from 
transitioning on the input states with the input symbol'''

    #Step instructions:
    #Compute the closure for each state reachable from the start list
    #but do not add duplicate states. For those steps that do not match
    #the step char, abandon them. For those that do, add their output 
    #to the list.
    
    eclosure = epsilonclosure(startstates)

    __next_states_result_set.clear()
    for state in eclosure:
        if state.c == inputc: __next_states_result_set.add(state.out)
    return __next_states_result_set

def simulate(machine,input):
    '''Simulate an NFA constructed from a postfix notation regular expression'''

    cstate = set([machine]) #current state
    for c in input: cstate = step(cstate,c)

    #Calculate the closure one last time since it is done only on the
    #start of an input. See step.
    #Check that there is at least one matching state.
    return not not filter(lambda state: state.c == -2, epsilonclosure(cstate))


class __dfa_state:
    '''A class representing a dfa state'''

    def __init__(self,states):

        #A set of NFA states. Really, a set of states in a NFA
        #that this single DFA state represents. You can think of a
        #set of NFA states as a single DFA state, where all the
        #reachable states from the NFA states for an input char
        #represent a new DFA state (the next DFA state)
        
        self.states = states
        
        #A map from input character to next dfa.
        self.next = {}
        
def dfa_state_lookup(dfa_state_list,state_set):
    '''Search for a cached dfa state that has the same set of states 
as the input set'''

    for dfastate in dfa_state_list:
        if dfastate.states == state_set: 
            print "\ndfa cache hit"
            return dfastate
    
    #Make a copy here since state_set is a global variable used for
    #calculations of states. Its function is to promote object reuse.
    new_dfa_state = __dfa_state(state_set.copy())

    dfa_state_list.add(new_dfa_state)
    return new_dfa_state

def dfasimulate(machine,input):
    '''Simulate a DFA machine, converting it from an NFA on the fly'''

    dfa_state_list = set()
    cstate = __dfa_state(set([machine]))
    dfa_state_list.add(cstate)

    for c in input: 
        if c not in cstate.next:
            nstate = dfa_state_lookup(dfa_state_list, 
                                      step(cstate.states,c))

            #Zero states were reachable from the current position for the latest
            #input character so for efficiency break and return a negative match
            if len(nstate.states) == 0: return False

            cstate.next[c] = nstate
        cstate = cstate.next[c]

    #Calculate the closure one last time and check that there is 
    #at least one matching state.
    return not not filter(lambda state: state.c == -2, 
                          epsilonclosure(cstate.states))

if __name__ == '__main__':
    result = re2post(sys.argv[1])
    print 'Out: %s' % result
    machine = post2nfa(result)
    print 'Match: %s' % simulate(machine,sys.argv[2])
    print 'Match: %s' % dfasimulate(machine,sys.argv[2])

