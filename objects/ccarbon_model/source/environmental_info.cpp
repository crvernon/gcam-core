/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file environmental_info.cpp
 * \ingroup objects
 * \brief EnvironmentalInfo class source file.
 * \author Jim Naslund and Ming Chang
 */
#include "util/base/include/definitions.h"
#include "ccarbon_model/include/environmental_info.h"
#include "climate/include/iclimate_model.h"
#include <typeinfo>

using namespace std;

/*!
 * \brief Default constructor.
 * \details Constructs the object and initializes the members.
 * \param aClimateModel the climate model.
 * \param aLandUse the land use.
 * \param aHistoricalShare the historical share.
 * \param aLandUseHistory the land use history.
 */
EnvironmentalInfo::EnvironmentalInfo( const IClimateModel* aClimateModel )
: mClimateModel( aClimateModel ), mLandUse( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{
}

/*! EnvironmentalInfo(const EnvironmentalInfo &aEnvironmentalInfo)
 * \brief a Copy Constructor for EnvironmentalInfo
 * \details create a EnvironmentalInfo object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param aEnvironmentalInfo const reference of a EnvironmentalInfo object
 * \author Ming Chang
 */
EnvironmentalInfo::EnvironmentalInfo(const EnvironmentalInfo &aEnvironmentalInfo)
: mClimateModel( aEnvironmentalInfo.mClimateModel ),
  mLandUseHistory( aEnvironmentalInfo.mLandUseHistory ),
  mLandUse( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
  {
    
    objects::PeriodVector<double>::const_iterator periodVecIterator;
    
    //! check if type matches before I copy the entire vector list.
    if ( typeid( aEnvironmentalInfo.mModeledLandUse ) == typeid( this->mModeledLandUse ) ){
        std::copy( aEnvironmentalInfo.mModeledLandUse.begin(),
                   aEnvironmentalInfo.mModeledLandUse.end(),
                   this->mModeledLandUse.begin() );
    }

    this->mHistoricalShare = aEnvironmentalInfo.mHistoricalShare;
    this->mKey = aEnvironmentalInfo.mKey;
}

EnvironmentalInfo::~EnvironmentalInfo(){
}
/*! EnvironmentalInfo::getTemperature( const int aYear ) const
 * \brief Returns the temperature for a year.
 * \details Returns the temperature for the passed in year.
 * \param aYear the year.
 * \return a double representing the temperature in the given year.
 */
const double EnvironmentalInfo::getTemperature( const int aYear ) const {
    return mClimateModel->getTemperature( aYear );
}

/*! objects::PeriodVector<double> EnvironmentalInfo::getModeledLandUse() const
 * \brief Returns the land use object.
 * \details Returns the land use object.
 * \return the land use object.
 * \todo Consider if this function pair can be replaced by set/getLandUse. Would need to change some
 *       interfaces since would not want to pass back the larger object.
 */
const objects::PeriodVector<double>& EnvironmentalInfo::getModeledLandUse() const {
    return mModeledLandUse;
}

/*! EnvironmentalInfo::setModeledLandUse( const double aLandUse, const int aPeriod )
 * \brief Sets the land use for a given period.
 * \details Sets the land use to the passed in value for the passed in period.
 * \param aLandUse to the land use.
 * \param aPeriod the working period.
 */
void EnvironmentalInfo::setModeledLandUse( const double aLandUse, const int aPeriod ){
    mModeledLandUse[ aPeriod  ] = aLandUse;
}

/*! 
 * \brief Returns the land used for a given year.
 * \details Returns the land used for a given year. This function is used as a utility
 *  function so that carbon box model compoments do not have to calculate land-use history
 *  more than once. 
 * \return the land used for a given year.
 * \author Steve Smith
 */
double EnvironmentalInfo::getLandUse( const int aYear ) const {
    if ( aYear >= CarbonModelUtils::getStartYear() ) {
        if ( aYear <= CarbonModelUtils::getEndYear() ) {
            return mLandUse[ aYear ];
        }
        else {
            return mLandUse[ CarbonModelUtils::getEndYear() ];
        }
    } 
    else {
        return mLandUse[ CarbonModelUtils::getStartYear() ];
    }
}

/*! 
 * \brief Sets the land use for a given year.
 * \details Sets the land use to the passed in value for a year.  This function 
 *  is used as a utility function so that carbon box model compoments do not have to calculate 
 *  land-use historymore than once. 
 * \param aLandUse to the land use.
 * \param aYear the year.
 * \author Steve Smith
 */
void EnvironmentalInfo::setLandUse( const double aLandUse, const int aYear ){
    mLandUse[ aYear ] = aLandUse;
}

/*! double EnvironmentalInfo::getHistoricalShare() const
 * \brief Returns the historical share object.
 * \details Returns the historical share object.
 * \return the historical share object.
 */
const double EnvironmentalInfo::getHistoricalShare() const {
    return mHistoricalShare;
}

/*! EnvironmentalInfo::setHistoricalShare( double aShare )
 * \brief Sets the historical share.
 * \details Sets the historical share.
 * \param aShare the share.
 */
void EnvironmentalInfo::setHistoricalShare( double aShare ){
    mHistoricalShare = aShare;
}
    
/*! LandUseHistory* EnvironmentalInfo::getLandUseHistory() const
 * \brief Returns the land use history object.
 * \details Returns the land use history  object.
 * \return the land use history  object.
 */
const LandUseHistory* EnvironmentalInfo::getLandUseHistory() const {
    return mLandUseHistory;
}

/*! EnvironmentalInfo::setLandUseHistory( const LandUseHistory* aLandUseHistory )
 * \brief Sets the land use history.
 * \details Sets the land use history.
 * \param aLandUseHistory the land use history.
 */
void EnvironmentalInfo::setLandUseHistory( const LandUseHistory* aLandUseHistory ){
    mLandUseHistory = aLandUseHistory;
}

/*! EnvironmentalInfo::setKey( const int aKey )
 * \brief Sets the key.
 * \details Sets the key that is unique to the CarbonBoxModel that owns this
            EnvironmentalInfo object.  This key is stored here so it can be
            easily accessed by objects the CarbonBoxModel owns such as CarbonBoxes
            and CarbonFlows.
 * \param aLandUseHistory the unique key.
 */
void EnvironmentalInfo::setKey( const int aKey ){
    mKey = aKey;
}

/*! EnvironmentalInfo::getKey() const
 * \brief Returns the key..
 * \details Returns the key that is unique to the CarbonBoxModel that owns this
            EnvironmentalInfo object.
 * \return the unique key.
 */
int EnvironmentalInfo::getKey() const {
    return mKey;
}
