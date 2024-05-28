/**
* Name: Sahil Kakadiya
* Student Number: 041052919
* Lab Section: 301
* Description: define constructor and deconstructor and private and public variabes
* Professor Name: Eric Torunski
*/
#include<iostream>

using namespace std;

namespace CST8219 {
    class Vehicle {
    private:
        int numWheels;
        int numDoors;

    public:
        // Constructor a) for the number of wheels and doors
        
        Vehicle(int w, int d) : numWheels(w), numDoors(d) {
            cout << "In constructor with 2 parameters" << endl;
        }

        // Constructor b) with wheels parameter and calling a)
        Vehicle(int w) : Vehicle(w, 4) {
            cout << "In constructor with 1 parameter, wheels = " << w << endl;
        }

        // Constructor c) empty constructor calling b)
        Vehicle() : Vehicle(4) {
            cout << "In constructor with 0 parameters" << endl;
        }

        // Destructor
        ~Vehicle() {
            cout << "In destructor" << endl;
        }
  
    };

    

  
}

int main(int argc, char **argv)
{
    CST8219::Vehicle myVehicle(4,2); // This calls constructor Vehicle(int,int)
    cout << "I made a vehicle" << endl;
    return 0;
}