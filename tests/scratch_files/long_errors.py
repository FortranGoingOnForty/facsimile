# Test file with errors that generate long diagnostic messages

from typing import Dict, List, Optional, Union, Callable

# Type mismatch with long type names
def process_data(
    data: Dict[str, List[Union[int, float, str, None]]]
) -> Dict[str, List[Union[int, float]]]:
    return data  # Error: incompatible return type

# Calling with wrong argument types
def complex_function(
    callback: Callable[[Dict[str, List[int]], Optional[str]], bool],
    items: List[Dict[str, Union[int, str, float, bool, None]]]
) -> None:
    pass

complex_function("not a callback", 123)  # Multiple type errors

# Undefined variable with suggestion
very_long_variable_name_that_might_be_misspelled = 42
print(very_long_variabel_name_that_might_be_mispelled)  # Typo - long suggestion

# Missing required arguments
def function_with_many_required_params(
    first_required_param: str,
    second_required_param: int,
    third_required_param: float,
    fourth_required_param: bool,
    fifth_required_param: List[str]
) -> None:
    pass

function_with_many_required_params()  # Missing all required arguments

# Incompatible override
class BaseClassWithLongMethodSignature:
    def very_descriptive_method_name_for_processing_data(
        self,
        input_data: Dict[str, List[int]],
        configuration: Optional[Dict[str, str]] = None
    ) -> List[Dict[str, Union[int, str]]]:
        return []

class DerivedClass(BaseClassWithLongMethodSignature):
    def very_descriptive_method_name_for_processing_data(
        self,
        input_data: str  # Wrong signature
    ) -> int:
        return 0
