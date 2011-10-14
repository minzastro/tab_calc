test_suite StringArray

test TStingTest1 !test assingments
  type(TString) :: x1, x2
  character*(20) s
  s = 'test'
  x1 = s
  x2 = s
  Assert_Equal('test', trim(x1%chars))
  x2 = x1 + x2
  Assert_Equal('testtest', trim(x2%chars))
end test

test TStringArrayTest
  character*(40) s
  integer i(6), is
  type(TStringArray) :: sa
  sa = TStringArraySplitX('x1,x2,x3,,x4', ',')
  Assert_Equal(len(sa), 5)
  sa = TStringArraySplitX('x1,x2,x3,,x4', ',', .true.)
  Assert_Equal(len(sa), 4)
  Assert_True(TStringArrayIn('x2', sa))
  Assert_False(TStringArrayIn('x22', sa))
  call TStringArrayAdd(sa, 'x5')
  Assert_Equal(len(sa), 5)
  call TStringArrayJoin(sa, '-', s)
  Assert_Equal(trim(s), 'x1-x2-x3-x4-x5')
  sa = TStringArraySplitX('1,4,3,1', ',')
  call toIntegerArray(sa, 6, i, is)
  Assert_Equal(is, 4)
  Assert_Equal(i(1), 1)
  Assert_Equal(i(3), 3)
end test

end test_suite
