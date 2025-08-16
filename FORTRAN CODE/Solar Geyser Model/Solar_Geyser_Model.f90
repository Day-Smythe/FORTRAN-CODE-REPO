
PROGRAM geyser_power_model
  IMPLICIT NONE

  ! Declare variables
  REAL :: current_temp        ! Current temperature of the geyser water (Celsius)
  REAL :: total_power_wh      ! Total power consumed in Watt-hours (Wh)
  REAL :: daily_water_liters  ! Total liters of hot water used for the day
  REAL :: water_usage_hourly  ! Liters of hot water used in a given hour
  INTEGER :: hour             ! Loop counter for the hour of the day
  INTEGER :: season_choice    ! User input to select the season (1 for Summer, 2 for Winter)
  INTEGER :: solar_start_hour ! Start hour for solar power availability
  INTEGER :: solar_end_hour   ! End hour for solar power availability
  INTEGER, PARAMETER :: file_unit = 10 ! File unit number for the CSV file

  ! Define constants for the geyser's characteristics
  REAL, PARAMETER :: SOLAR_SETPOINT = 65.0      ! Target temperature on solar power (C)
  REAL, PARAMETER :: GRID_SETPOINT = 55.0       ! Target temperature on grid power (C)

  REAL, PARAMETER :: SOLAR_HEATING_KWH = 0.5   ! kWh to heat from 39C to 65C (solar)
  REAL, PARAMETER :: GRID_HEATING_KWH = 0.25    ! kWh to heat from 39C to 55C (grid)

  REAL, PARAMETER :: SOLAR_MAINTENANCE_W = 0.9  ! Watts to maintain 65C (solar)
  REAL, PARAMETER :: GRID_MAINTENANCE_W = 0.21   ! Watts to maintain 55C (grid)

  REAL, PARAMETER :: WH_PER_LITER = 0.41      ! Wh used per liter of water drawn
  
  REAL, PARAMETER :: TEMP_DROP_PER_LITER = 0.2

  ! Define constants for natural temperature loss
  REAL, PARAMETER :: DAY_LOSS_55C = 0.01   ! Degrees C lost per hour at 55C during the day
  REAL, PARAMETER :: NIGHT_LOSS_55C = 0.09 ! Degrees C lost per hour at 55C during the night
  REAL, PARAMETER :: DAY_LOSS_65C = 0.05   ! Degrees C lost per hour at 65C during the day
  REAL, PARAMETER :: NIGHT_LOSS_65C = 0.25 ! Degrees C lost per hour at 65C during the night

  ! Initialize variables
  current_temp = GRID_SETPOINT      ! Assume the geyser starts at the grid setpoint
  total_power_wh = 0.0
  daily_water_liters = 0.0

  ! Ask the user for the season
  WRITE(*,*) "--------------------------------------------------------"
  WRITE(*,*) "Is it summer or winter?"
  WRITE(*,*) "Enter 1 for Summer, or 2 for Winter:"
  READ(*,*) season_choice
  WRITE(*,*) "--------------------------------------------------------"

  ! Set solar power hours based on the user's choice
  IF (season_choice == 1) THEN
    ! Summer solar hours: 05:30 to 18:50. For this hourly model, we'll approximate to 05:00 to 18:00.
    solar_start_hour = 5
    solar_end_hour = 18
    WRITE(*,*) "Simulating for summer with solar hours from ", solar_start_hour, ":00 to ", solar_end_hour, ":00."
  ELSE IF (season_choice == 2) THEN
    ! Winter solar hours: 06:00 to 17:00
    solar_start_hour = 6
    solar_end_hour = 17
    WRITE(*,*) "Simulating for winter with solar hours from ", solar_start_hour, ":00 to ", solar_end_hour, ":00."
  ELSE
    WRITE(*,*) "Invalid choice. Defaulting to winter."
    solar_start_hour = 6
    solar_end_hour = 17
  END IF
  WRITE(*,*) "Initial Temperature: ", current_temp, " C"
  WRITE(*,*) "--------------------------------------------------------"

  ! Open the CSV file for writing
  OPEN(UNIT=file_unit, FILE='geyser_data.csv', STATUS='REPLACE')

  ! Write the header line to the CSV file
  WRITE(file_unit, '(A, A, A, A)') 'Hour,', 'Current_Temp_C,', &
                                  'Hourly_Water_Liters,', 'Total_Power_Wh'

  ! Loop through each hour of the day
  DO hour = 0, 23

    ! Simulate natural temperature loss first
    ! The loss rate depends on the current temperature and the time of day (day vs. night)
    IF (hour >= solar_start_hour .AND. hour <= solar_end_hour) THEN
      ! Daytime temperature loss
      IF (current_temp >= SOLAR_SETPOINT) THEN
        current_temp = current_temp - DAY_LOSS_65C
      ELSE
        current_temp = current_temp - DAY_LOSS_55C
      END IF
    ELSE
      ! Nighttime temperature loss
      IF (current_temp >= SOLAR_SETPOINT) THEN
        current_temp = current_temp - NIGHT_LOSS_65C
      ELSE
        current_temp = current_temp - NIGHT_LOSS_55C
      END IF
    END IF
    
    ! Check if solar power is available for the current hour
    IF (hour >= solar_start_hour .AND. hour <= solar_end_hour) THEN
      ! Geyser is on solar power
      WRITE(*,*) "HOUR ", hour, ": SOLAR POWER AVAILABLE"

      ! Check if the temperature has dropped below the solar setpoint
      IF (current_temp < SOLAR_SETPOINT) THEN
        ! Need to heat the water
        total_power_wh = total_power_wh + (SOLAR_HEATING_KWH * 1000.0)
        current_temp = SOLAR_SETPOINT
        WRITE(*,*) "  > Heating to ", SOLAR_SETPOINT, "C. Adding ", SOLAR_HEATING_KWH, " kWh."
      ELSE
        ! Temperature is at or above the setpoint, use maintenance power
        total_power_wh = total_power_wh + SOLAR_MAINTENANCE_W
        WRITE(*,*) "  > Maintaining temperature. Adding ", SOLAR_MAINTENANCE_W, " Wh."
      END IF

    ELSE
      ! Geyser is on grid power
      WRITE(*,*) "HOUR ", hour, ": GRID POWER IN USE"

      ! Check if the temperature has dropped below the grid setpoint
      IF (current_temp < GRID_SETPOINT) THEN
        ! Need to heat the water
        total_power_wh = total_power_wh + (GRID_HEATING_KWH * 1000.0)
        current_temp = GRID_SETPOINT
        WRITE(*,*) "  > Heating to ", GRID_SETPOINT, "C. Adding ", GRID_HEATING_KWH, " kWh."
    
      END IF
    END IF

    water_usage_hourly = 0.0
    IF (hour == 6) THEN
      water_usage_hourly = 5.0
    ELSE IF (hour == 12) THEN
      water_usage_hourly = 10.0
    ELSE IF (hour == 18) THEN
      water_usage_hourly = 20.0
    ELSE IF (hour == 20) THEN
      water_usage_hourly = 120.0
    END IF

    IF (water_usage_hourly > 0.0) THEN
      total_power_wh = total_power_wh + (water_usage_hourly * WH_PER_LITER)
      daily_water_liters = daily_water_liters + water_usage_hourly
      current_temp = current_temp - (water_usage_hourly * TEMP_DROP_PER_LITER)
      WRITE(*,*) "  > Hot water used: ", water_usage_hourly, " liters. Temp drops to ", current_temp, " C."
    END IF

    ! Write the data for the current hour to the CSV file
    WRITE(file_unit, '(I0, A, F6.2, A, F5.1, A, F8.2)') &
           hour, ',', current_temp, ',', water_usage_hourly, ',', total_power_wh

    WRITE(*,*) "  > End of Hour ", hour, ": Current Temperature: ", current_temp, " C"
    WRITE(*,*) "--------------------------------------------------------"

  END DO

  CLOSE(UNIT=file_unit)

  WRITE(*,*) "Simulation complete."
  WRITE(*,*) "Total hot water used for the day: ", daily_water_liters, " liters"
  WRITE(*,*) "Total power consumed for the day: ", total_power_wh, " Wh"
  WRITE(*,*) "Total power consumed for the day: ", total_power_wh / 1000.0, " kWh"
  WRITE(*,*) "A log of the data has been saved to geyser_data.csv"

END PROGRAM geyser_power_model
