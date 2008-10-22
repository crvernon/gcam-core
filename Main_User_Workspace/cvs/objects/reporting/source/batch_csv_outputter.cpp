/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*!
* \file batch_csv_outputter.cpp
* \ingroup Objects
* \brief The BatchCSVOutputter class source file for writing batch results to a csv file.
* \details This source file contains the definition for the startVisit and endVisit methods
*          for each class that the visitor visits.  Values will be written directly to the
*          file specified by the configuration paramater batchCSVOutputFile.
* \todo Figure out why I need a string to output doubles/chars to an AutoOutputFile.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"

#include "util/base/include/configuration.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "marketplace/include/market.h"
#include "marketplace/include/imarket_type.h"
#include "climate/include/iclimate_model.h"

#include <string>

#include "reporting/include/batch_csv_outputter.h"


extern Scenario* scenario;

using namespace std;

/*! \brief Constructor
*/
BatchCSVOutputter::BatchCSVOutputter():
mFile( "batchCSVOutputFile", "batch-csv-out.csv" ),
mIsFirstScenario(true)
{
}

/*!
 * \brief Destructor
 */
BatchCSVOutputter::~BatchCSVOutputter(){
}


void BatchCSVOutputter::startVisitScenario( const Scenario* aScenario, const int aPeriod ) {

    // only output the header info one time
    // we can not put this in the constructor because we will not have a model time
    // a that point
    if( mIsFirstScenario ) {
        mFile << "Scenario" << ',';
        const Modeltime* modeltime = aScenario->getModeltime();

        for( int period = 0; period < modeltime->getmaxper(); ++period ) {
            // TODO: hard coding CO2
            const int year = modeltime->getper_to_yr( period );
            mFile << year << ' '<< "CO2 Price" << ',';
            mFile << year << ' '<< "CO2 Emissions" << ',';
        }

        for( int period = 0; period < modeltime->getmaxper(); ++period ) {
            // TODO: hard coding CO2
            const int year = modeltime->getper_to_yr( period );
            mFile << year << ' '<< "CO2 Concentration" << ',';
            mFile << year << ' '<< "CO2 Radiative Forcing" << ',';
            mFile << year << ' '<< "CO2 Temperature Change" << ',';
        }
        mFile << "Solved" << endl;
    }
    mIsFirstScenario = false;

    mFile << aScenario->getName() << ',';
    // TODO: perhaps write some date/time or something
}

void BatchCSVOutputter::startVisitMarket( const Market* aMarket, const int aPeriod ) {
    if( aMarket->getType() == IMarketType::TAX ) {
        /*!
         * \warninng This is assuming the periods will be visited in appropriate order.
         */
        mFile << "" << aMarket->getPrice() << ',';
        
        // would this be wrong if it didn't solve?
        mFile << "" << aMarket->getDemand() << ',';
    }
}

void BatchCSVOutputter::startVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod ) {
    int outputInterval
        = Configuration::getInstance()->getInt( "climateOutputInterval",
                                   scenario->getModeltime()->gettimestep( 0 ) );

    // print at least to 2100 if interval is set appropriately
    int endingYear = max( scenario->getModeltime()->getEndYear(), 2100 );

    // Write the concentrations for the request period.
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
        mFile << "" << aClimateModel->getConcentration( "CO2", year ) << ',';
        mFile << "" << aClimateModel->getForcing( "CO2", year) << ',';
        mFile << "" << aClimateModel->getTemperature( year ) << ',';
    }
}

/*!
 * \brief Writes whether the current scenario had solved or not.
 * \param aDidSolve Whether the current scenario solved.
 */
void BatchCSVOutputter::writeDidScenarioSolve( bool aDidSolve ) {
    mFile << aDidSolve << endl;
}