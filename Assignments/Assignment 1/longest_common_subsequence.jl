function lcs(s1,s2)
  A=[]
  for c in s1
    push!(A,c)
  end
  B=[]
  for c in s2
    push!(B,c)
  end
  n = length(A)
  m = length(B)
  dptable = Array{Int64,2}(n+1,m+1)
  for i = 0:n
    for j = 0:m
      if i == 0 || j == 0
        dptable[i+1,j+1] = 0
      elseif A[i] == B[j]
        dptable[i+1,j+1] = dptable[i,j] + 1
      else
        dptable[i+1,j+1] = max(dptable[i+1,j], dptable[i,j+1])
      end
    end
  end
  len = dptable[n+1,m+1]
  seq = Array{Char,1}(len)
  i, j = n+1, m+1
  while len != 0
    if A[i-1] == B[j-1]
      seq[len] = A[i-1]
      len -= 1
      i -= 1
      j -= 1
    elseif dptable[i,j-1] > dptable[i-1,j]
      j -= 1
    else
      i -= 1
    end
  end
  join(seq)
end
