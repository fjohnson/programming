def swap(A,t1,t2):
 t = A[t1]
 A[t1] = A[t2]
 A[t2] = t

def partition(A,s,e):
  p = (s+e) / 2
  swap(A,p,e)
  pos = s
  for i in xrange(s,e):
    if A[i] < A[e]:
      swap(A,pos,i)
      pos = pos + 1
  swap(A,pos,e)
  return p

def qsort(A,s,e):
  if s == e: return
  p = partition(A,s,e)
  qsort(A,s,p-1)
  qsort(A,p,e)

print qsort([4,3,2,1],0,3)
print qsort([9,2,15,21,54,1,8,4,4,2,0,9],0,11)
