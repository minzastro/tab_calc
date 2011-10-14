test_suite array_works

integer iX, iY

test ArraySequenceTest
  real*8 x(5)
  call ArraySequence(x, 5, 0d0, 1d0)
  Assert_Equal(x(2), 1d0)
end test

test RemoveDelimitersTest
  character*(100) sLine
  cDelimiter = ','
  sLine = 'test,,test,test,test,,test'
  call RemoveDelimiters(sLine, .true., .false.)
  Assert_Equal(trim(sLine), 'test,test,test,test,test')
end test

test LoadFromFileTest
  call LoadFromFile('data_simple', datatable, iX, iY)
  Assert_Equal(iX, 3)
  Assert_Equal(iY, 2)
  iSkipAmount = 1
  call LoadFromFile('data_simple', datatable, iX, iY)
  Assert_Equal(iX, 3)
  Assert_Equal(iY, 1)
  iSkipAmount = 0
end test

test LoadFromFileExtTest
  xcol_ignore_num = 1
  xcol_ignore(1) = 2
  iIgnoranceMode = 1
  call LoadFromFileExt('data_simple', datatable, iX, iY)
  Assert_Equal(datatable(1, 2), 0d0)
end test

test LoadFromFileAlignedTest
  cDelimiter = '|'
  call LoadFromFileAligned('data_aligned', iX, iY)
  Assert_Equal(datatable(2,2), 3d0)
end test

end test_suite
