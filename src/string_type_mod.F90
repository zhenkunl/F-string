module string_type_mod

  implicit none

  private

  type, public :: string
    private
    character(len=:), allocatable :: value
  contains
    private
    generic, public                :: assignment(=) => assign_character_to_string, assign_string_to_character, &
                                      assign_string_to_string
    procedure, private, pass(lhs)  :: assign_character_to_string
    procedure, private, pass(rhs)  :: assign_string_to_character
    procedure, private, pass(lhs)  :: assign_string_to_string
    procedure, public, pass(self)  :: get_value
    procedure, public, pass(self)  :: len      => len_string
    procedure, public, pass(self)  :: len_trim => len_trim_string
    procedure, public, pass(self)  :: trim     => trim_string
    procedure, public, pass(self)  :: adjustl  => adjustl_string
    procedure, public, pass(self)  :: adjustr  => adjustr_string
    procedure, public, pass(self)  :: reverse  => resverse_string
    procedure, public, pass(self)  :: to_lower => to_lower_string
    procedure, public, pass(self)  :: to_upper => to_upper_string
    procedure, public, pass(self)  :: to_int   => string_to_int
    procedure, public, pass(self)  :: capitalize
    procedure, public, pass(self)  :: count
    procedure, public, pass(self)  :: find
    procedure, public, pass(self)  :: start_with
    procedure, public, pass(self)  :: end_with
    generic, public                :: join     => join_characters, join_strings
    procedure, private, pass(self) :: join_characters
    procedure, private, pass(self) :: join_strings
    generic, public                :: operator(//) => string_concat_string, string_concat_character, character_concat_string
    procedure, private, pass(lhs)  :: string_concat_string
    procedure, private, pass(lhs)  :: string_concat_character
    procedure, private, pass(rhs)  :: character_concat_string
#ifdef __GNUC__
    procedure, public, pass(self)  :: delete => delete_string_polymorph
#endif
    final :: delete_string
  end type string

  interface string
    module procedure new_string_from_str
    module procedure new_string_from_int
  end interface string

contains

  function new_string_from_str(val) result(new)

    implicit none
    character(len=*), intent(in) :: val
    type(string)                 :: new

    new%value = val

  end function new_string_from_str

  function new_string_from_int(val) result(new)

    implicit none
    integer, intent(in)       :: val
    type(string)              :: new
    integer, parameter        :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer

    write(buffer, '(i0)') val
    new%value = trim(buffer)

  end function new_string_from_int

  subroutine delete_string(self)

    implicit none
    type(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string

#ifdef __GNUC__
  subroutine delete_string_polymorph(self)

    implicit none
    class(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string_polymorph
#endif

  subroutine assign_character_to_string(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    character(len=*), intent(in) :: rhs

    lhs%value = rhs

  end subroutine assign_character_to_string

  subroutine assign_string_to_character(lhs, rhs)

    implicit none
    character(len=:), allocatable, intent(out) :: lhs
    class(string), intent(in)                  :: rhs

    lhs = rhs%value

  end subroutine assign_string_to_character

  subroutine assign_string_to_string(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    type(string), intent(in)     :: rhs

    lhs%value = rhs%value

  end subroutine assign_string_to_string

  function get_value(self) result(string_value)

    implicit none
    class(string), intent(in)     :: self
    character(len=:), allocatable :: string_value

    string_value = self%value

  end function get_value

  function len_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len(self%value)

  end function len_string

  function len_trim_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len_trim(self%value)

  end function len_trim_string

  function trim_string(self) result(trimmed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: trimmed_string

    trimmed_string = trim(self%value)

  end function trim_string

  function adjustl_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = adjustl(self%value)

  end function adjustl_string

  function adjustr_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = adjustr(self%value)

  end function adjustr_string

  function resverse_string(self) result(reversed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: reversed_string
    integer                   :: i, n

    reversed_string = self
    n = self%len()
    do i = 1, n
      reversed_string%value(n-i+1:n-i+1) = self%value(i:i)
    end do

  end function resverse_string

  function to_lower_string(self) result(lower_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: lower_string
    character(len=*), parameter :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                     :: i, j

    lower_string = self
    do i = 1, self%len()
      j = index(upper_alphabet, lower_string%value(i:i))
      if (j > 0) then
        lower_string%value(i:i) = lower_alphabet(j:j)
      end if
    end do

  end function to_lower_string

function to_upper_string(self) result(upper_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: upper_string
    character(len=*), parameter :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                     :: i, j

    upper_string = self
    do i = 1, self%len()
      j = index(lower_alphabet, upper_string%value(i:i))
      if (j > 0) then
        upper_string%value(i:i) = upper_alphabet(j:j)
      end if
    end do

  end function to_upper_string

  function capitalize(self) result(capitalized_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: capitalized_string
    character(len=*), parameter :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer                     :: i

    capitalized_string = self%to_lower()
    i = index(lower_alphabet, capitalized_string%value(1:1))
    if (i > 0) then
      capitalized_string%value(1:1) = upper_alphabet(i:i)
    end if

  end function capitalize

  function count(self, substring) result(number)

    implicit none
    class(string), intent(in) :: self
    character(*), intent(in)  :: substring
    integer                   :: number
    integer                   :: start, idx

    number = 0
    if (len(substring) < self%len()) then
      start = 1
      do
        idx = index(self%value(start:), substring)
        if (idx == 0) then
          exit
        else
          number = number + 1
        end if
        start = start + idx + len(substring) - 1
      end do
    end if

  end function count

  function find(self, substring, back) result(idx)

    implicit none
    class(string), intent(in)     :: self
    character(*), intent(in)      :: substring
    logical, intent(in), optional :: back
    integer                       :: idx

    if (len(substring) < self%len()) then
      idx = index(self%value, substring, back)
    else
      idx = 0
    end if

  end function find

  function start_with(self, prefix, start, end) result(res)

    implicit none
    class(string), intent(in)     :: self
    character(*), intent(in)      :: prefix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = index(self%value(start_:end_), prefix) == 1

  end function start_with

  function end_with(self, suffix, start, end) result(res)

    implicit none
    class(string), intent(in)     :: self
    character(*), intent(in)      :: suffix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = self%value(end_-len(suffix)+1:end_) == suffix

  end function end_with

  function join_characters(self, array) result(join)

    implicit none
    class(string), intent(in)    :: self
    character(len=*), intent(in) :: array(:)
    type(string)                 :: join
    integer                      :: i

    join = array(1)
    do i = 2, size(array)
      join%value = join%value//self%value//array(i)
    end do

  end function join_characters

  function join_strings(self, array) result(join)

    implicit none
    class(string), intent(in) :: self
    type(string), intent(in)  :: array(:)
    type(string)              :: join
    integer                   :: i

    join = array(1)
    do i = 2, size(array)
      join%value = join%value//self%value//array(i)%value
    end do

  end function join_strings

  function string_concat_string(lhs, rhs) result(concat)

    implicit none
    class(string), intent(in)     :: lhs
    type(string),  intent(in)     :: rhs
    character(len=:), allocatable :: concat

    concat = lhs%value//rhs%value

  end function string_concat_string

  function string_concat_character(lhs, rhs) result(concat)

    implicit none
    class(string), intent(in)     :: lhs
    character(len=*), intent(in)  :: rhs
    character(len=:), allocatable :: concat

    concat = lhs%value//rhs

  end function string_concat_character

  function character_concat_string(lhs, rhs) result(concat)

    implicit none
    character(len=*), intent(in)  :: lhs
    class(string), intent(in)     :: rhs
    character(len=:), allocatable :: concat

    concat = lhs//rhs%value

  end function character_concat_string

  function string_to_int(self) result(int)

    implicit none
    class(string), intent(in) :: self
    integer                   :: int

    read(self%value, *) int

  end function string_to_int

end module string_type_mod