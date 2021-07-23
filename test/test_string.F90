program test_string

  use iso_fortran_env, only : i1 => int8, i2 => int16, i4 => int32, i8 => int64, r4 => real32, r8 => real64
  use string, only: string_t
  use unittest, only: unittest_t
  implicit none

  type(string_t)   :: str1, str2, str3
  type(unittest_t) :: test

  call test%init('StringTest')

  call test%start_case('TestConstructor')
  str1 = string_t('abc ')
  call test%assert_equal(str1%value(), 'abc', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  call test%assert_equal(str1%len_trim(), 3, __FILE__, __LINE__)
  str1 = string_t('123')
  call test%assert_equal(str1%value(), '123', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 3, __FILE__, __LINE__)
  str1 = string_t(.true.)
  call test%assert_equal(str1%value(), 'True', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  str1 = string_t(.false.)
  call test%assert_equal(str1%value(), 'False', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 5, __FILE__, __LINE__)
  str1 = string_t(0.0123456789, 3)
  call test%assert_equal(str1%value(), '0.123E-01', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 9, __FILE__, __LINE__)
  str1 = string_t(-0.0123456789, 3)
  call test%assert_equal(str1%value(), '-0.123E-01', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 10, __FILE__, __LINE__)
  str1 = string_t(0.123456789, 3)
  call test%assert_equal(str1%value(), '0.123', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 5, __FILE__, __LINE__)
  str1 = string_t(-0.123456789, 3)
  call test%assert_equal(str1%value(), '-0.123', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 6, __FILE__, __LINE__)
  str1 = string_t(1.23456789, 3)
  call test%assert_equal(str1%value(), '1.23', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  str1 = string_t(-1.23456789, 3)
  call test%assert_equal(str1%value(), '-1.23', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 5, __FILE__, __LINE__)
  str1 = string_t(12.3456789, 3)
  call test%assert_equal(str1%value(), '12.3', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  str1 = string_t(-12.3456789, 3)
  call test%assert_equal(str1%value(), '-12.3', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 5, __FILE__, __LINE__)
  str1 = string_t(123.456789, 3)
  call test%assert_equal(str1%value(), '123.', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  str1 = string_t(-123.456789, 3)
  call test%assert_equal(str1%value(), '-123.', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 5, __FILE__, __LINE__)
  str1 = string_t(1234.56789, 3)
  call test%assert_equal(str1%value(), '0.123E+04', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 9, __FILE__, __LINE__)
  str1 = string_t(-1234.56789, 3)
  call test%assert_equal(str1%value(), '-0.123E+04', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 10, __FILE__, __LINE__)
  str1 = string_t(0.0123456789)
  call test%assert_equal(str1%value(), '1.2345679E-02', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 13, __FILE__, __LINE__)
  str1 = string_t(0.123456789)
  call test%assert_equal(str1%value(), '0.1234568', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 9, __FILE__, __LINE__)
  str1 = string_t(123.456789)
  call test%assert_equal(str1%value(), '123.4568', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 8, __FILE__, __LINE__)
  str1 = string_t(0.01234)
  call test%assert_equal(str1%value(), '1.2340000E-02', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 13, __FILE__, __LINE__)
  str1 = string_t(0.1234)
  call test%assert_equal(str1%value(), '0.1234000', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 9, __FILE__, __LINE__)
  str1 = string_t(123.4)
  call test%assert_equal(str1%value(), '123.4000', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 8, __FILE__, __LINE__)
  str1 = string_t(123.4_r4, 3)
  call test%assert_equal(str1%value(), '123.', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  str1 = string_t(123.4_r8, 3)
  call test%assert_equal(str1%value(), '123.', __FILE__, __LINE__)
  call test%assert_equal(str1%len(), 4, __FILE__, __LINE__)
  call test%end_case()

  call test%start_case('TestOperation')
  str1 = 'aBcD'
  str2 = str1%reverse()
  call test%assert_equal(str2%value(), 'DcBa', __FILE__, __LINE__)
  call test%assert_equal(str2%len(), 4, __FILE__, __LINE__)
  str2 = str1%to_lower()
  call test%assert_equal(str2%value(), 'abcd', __FILE__, __LINE__)
  call test%assert_equal(str2%len(), 4, __FILE__, __LINE__)
  str2 = str1%to_upper()
  call test%assert_equal(str2%value(), 'ABCD', __FILE__, __LINE__)
  call test%assert_equal(str2%len(), 4, __FILE__, __LINE__)
  str2 = str1%capitalize()
  call test%assert_equal(str2%value(), 'Abcd', __FILE__, __LINE__)
  call test%assert_equal(str2%len(), 4, __FILE__, __LINE__)
  str1 = '0123456123456123456123456'
  call test%assert_equal(str1%count('123'), 4, __FILE__, __LINE__)
  call test%assert_equal(str1%index('34'), 4, __FILE__, __LINE__)
  call test%assert_equal(str1%index('56', .true.), 24, __FILE__, __LINE__)
  str1 = 'abc'
  str2 = str1%repeat(3)
  call test%assert_equal(str2%value(), 'abcabcabc', __FILE__, __LINE__)
  str1 = 'abcdefg'
  call test%assert_equal(str1%scan('cf'), 3, __FILE__, __LINE__)
  call test%assert_equal(str1%verify('ad'), 2, __FILE__, __LINE__)
  str1 = '1234567'
  str2 = str1%at(2, 3)
  call test%assert_equal(str2%value(), '23', __FILE__, __LINE__)
  str1 = '123log.txt'
  call test%assert_equal(str1%start_with('123'), .true., __FILE__, __LINE__)
  call test%assert_equal(str1%start_with('og', 5, 15), .true., __FILE__, __LINE__)
  call test%assert_equal(str1%end_with('.txt'), .true., __FILE__, __LINE__)
  call test%assert_equal(str1%start_with('txt', 8, 20), .true., __FILE__, __LINE__)
  str1 = '123'
  str2 = '456'
  str3 = str1 // str2
  call test%assert_equal(str3%value(), '123456', __FILE__, __LINE__)
  str3 = str1 // '456'
  call test%assert_equal(str3%value(), '123456', __FILE__, __LINE__)
  str3 = '123' // str2
  call test%assert_equal(str3%value(), '123456', __FILE__, __LINE__)
  call test%end_case()

  call test%start_case('TestCompare')
  str1 = '123'
  str2 = '123 '
  call test%assert_true(str1 == str2, __FILE__, __LINE__)
  str2 = ' 123'
  call test%assert_true(str1 /= str2, __FILE__, __LINE__)
  str1 = '789'
  call test%assert_true(str1 == '789', __FILE__, __LINE__)
  call test%assert_true('789' == str1, __FILE__, __LINE__)
  call test%assert_true(str1 > '788', __FILE__, __LINE__)
  call test%assert_true('788' < str1, __FILE__, __LINE__)
  call test%assert_true(str1 < '790', __FILE__, __LINE__)
  call test%assert_true('790' > str1, __FILE__, __LINE__)
  call test%assert_true(str1 >= '789', __FILE__, __LINE__)
  call test%assert_true('789' <= str1, __FILE__, __LINE__)
  call test%assert_true(str1 <= '789', __FILE__, __LINE__)
  call test%assert_true('789' >= str1, __FILE__, __LINE__)
  call test%end_case()

  call test%summary()

end program test_string