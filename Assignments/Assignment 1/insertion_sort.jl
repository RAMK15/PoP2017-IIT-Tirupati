function insertion_sort(array)
  for i = 2:length(array)
    j = i-1; temp = array[i]
    while  j>=1 && array[j]>temp
      array[j+1] = array[j]
      j -= 1
    end
    array[j+1] = temp
  end
  return array
end
