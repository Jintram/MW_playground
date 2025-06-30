
# assignment:
# rewrite this function to a more modular form

def convert_temperature(temperature, unit):
    if unit == "F":
        # Convert Fahrenheit to Celsius
        celsius = (temperature - 32) * (5 / 9)
        if celsius < -273.15:
            # Invalid temperature, below absolute zero
            return "Invalid temperature"
        else:
            # Convert Celsius to Kelvin
            kelvin = celsius + 273.15
            if kelvin < 0:
                # Invalid temperature, below absolute zero
                return "Invalid temperature"
            else:
                fahrenheit = (celsius * (9 / 5)) + 32
                if fahrenheit < -459.67:
                    # Invalid temperature, below absolute zero
                    return "Invalid temperature"
                else:
                    return celsius, kelvin
    elif unit == "C":
        # Convert Celsius to Fahrenheit
        fahrenheit = (temperature * (9 / 5)) + 32
        if fahrenheit < -459.67:
            # Invalid temperature, below absolute zero
            return "Invalid temperature"
        else:
            # Convert Celsius to Kelvin
            kelvin = temperature + 273.15
            if kelvin < 0:
                # Invalid temperature, below absolute zero
                return "Invalid temperature"
            else:
                return fahrenheit, kelvin
    elif unit == "K":
        # Convert Kelvin to Celsius
        celsius = temperature - 273.15
        if celsius < -273.15:
            # Invalid temperature, below absolute zero
            return "Invalid temperature"
        else:
            # Convert Celsius to Fahrenheit
            fahrenheit = (celsius * (9 / 5)) + 32
            if fahrenheit < -459.67:
                # Invalid temperature, below absolute zero
                return "Invalid temperature"
            else:
                return celsius, fahrenheit
    else:
        return "Invalid unit"
    

# my answer below, 
# but see collaborative doc for very nice answer


# Convert two others to celcius
def convert_fahrenheit_to_celsius(fahrenheit):    
    # Convert Fahrenheit to Celsius
    return (fahrenheit - 32) * (5 / 9)

def convert_kelvin_to_celsius(kelvin):          
    # Convert Kelvin to Celsius
    return kelvin - 273.15
    
# Convert celsius to two others
def convert_celsius_to_kelvin(celsius):
    # Convert Celsius to Kelvin
    return celsius + 273.15
    
def connvert_celsius_to_fahrenheit(celsius):
    # convert celsius to fahrenheit
    return (celsius * (9 / 5)) + 32


def convert_temparature_MW(temperature, inputunit):
    ''' 
    input:
    - temperature: temperature in any unit
    - inputunit: the unit of that temperature
    output:
    This function will return the two other units of the input temperature
    
    To do: i would make this function output all three units, ie return [F, C, K] '''
    
    # Depending on the input unit, convert first to celsius if necessary, and then to the remaining unit(s).
    if inputunit == "F":
        
        # conversion
        celsius = convert_fahrenheit_to_celsius(temperature)
        
        # check if below absolute zero
        if celsius < -273.15:
            return "Invalid temperature"
        
        # conversion
        kelvin = convert_celsius_to_kelvin(celsius)

        return celsius, kelvin
    
    elif inputunit == "C":
        
        celsius = temperature
        
        # check if below absolute zero
        if celsius < -273.15:
            return "Invalid temperature"
        
        # conversion
        kelvin = convert_celsius_to_kelvin(temperature)
        fahrenheit = connvert_celsius_to_fahrenheit(temperature)
                
        return fahrenheit, kelvin
    
    elif inputunit == "K":

        # conversion
        celsius = convert_kelvin_to_celsius(temperature)
        
        # check if below absolute zero
        if celsius < -273.15:
            return "Invalid temperature"
        
        # conversion
        fahrenheit = connvert_celsius_to_fahrenheit(celsius)

        return celsius, fahrenheit
    
    else:
        
        return "Invalid unit"
    
convert_temparature_MW(200, "K")
convert_temparature_MW(200, "C")
convert_temparature_MW(200, "F")