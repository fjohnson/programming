def makeheap(A):
  start = len(A) / 2 - 1
  while start != -1: 
      heapify(A,start,len(A)-1)
      start = start - 1

def heapify(A,start,end):
    
  left = start * 2 + 1
  right = left + 1
  target = start
  if left <= end and A[left] > A[target]:
      target = left
  if right <= end and A[right] > A[target]:
      target = right
  if target != start:
      t = A[start]
      A[start] = A[target]
      A[target] = t
      heapify(A,target,end)

def heapsort(A):
  for i in range(len(A)):
      t = A[0]
      A[0] = A[len(A)-i-1]
      A[len(A)-i-1] = t
      if len(A)-i-2 > -1: heapify(A,0,len(A)-i-2)

#[16,10,14,2,3,5]
a = [5,3,16,2,10,14]
makeheap(a)
heapsort(a)
#print a
b = []
heapsort(b)
print b
