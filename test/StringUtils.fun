test_suite StringUtils

test TrimLeftTest
character*(10) sInput, sOut
  sInput = '   Zzz'
  call TrimLeft(sInput, sOut)
  Assert_Equal(trim(sOut), 'Zzz')
  sInput = 'Zz1 z'
  call TrimLeft(sInput, sOut)
  Assert_Equal(trim(sOut), 'Zz1 z')
end test

test sqeezeTest
character*(10) sInput, sOut
  sInput = '   Zzz'
  call sqeeze(sInput, 'z', sOut)
  Assert_Equal(trim(sOut), '   Zz')
  sInput = '   Zzzz zzz'
  call sqeeze(sInput, 'z', sOut)
  Assert_Equal(trim(sOut), '   Zz z')
end test

end test_suite
