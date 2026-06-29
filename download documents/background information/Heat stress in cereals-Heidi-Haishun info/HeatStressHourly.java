/*
 *  SIMPLACE - Scientific Impact assessment and Modelling PLattform for Advanced Crop and Ecosystem management
 *
 *  This uses the SIMPLACE utility.
 *  
 *  This file may contain modules that are subject of copyright laws and have to be cited accordingly.
 *  
 *  SIMPLACE framework is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  SIMPLACE is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with SIMPLACE.  If not, see <http://www.gnu.org/licenses/>.
 *  
 *  PhotosynthesisSimple.java
 *
 *  Responsible developers: Gunther Krauss & Andreas Enders, Crop Science Group, Katzenburgweg 5, 53115 Bonn, Germany
 *  Contact Information:                    lap@uni-bonn.de
 */

package net.simplace.client.simulation.lap.experimental.canopytemperature;


import java.util.HashMap;

import net.simplace.simulation.model.FWSimComponent;
import net.simplace.simulation.util.FWSimVarMap;
import net.simplace.simulation.util.FWSimVariable;
import net.simplace.simulation.util.FWSimVariable.CONTENT_TYPE;
import net.simplace.simulation.util.FWSimVariable.DATA_TYPE;

import org.jdom.Element;


public class HeatStressHourly extends FWSimComponent
{

  // Definition of constants
	FWSimVariable<Double> cTCritical; //Temperature at which reduction in final yield occurs due to kernel abortion [C]
		
	FWSimVariable<Double> cBeforeTSUM1; //Start of sensitive period for kernel abortion; 250DD before silking
	FWSimVariable<Double> cAfterTSUM1;  //End of sensitive period for kernel abortion; 100DD after silking
	FWSimVariable<Double> cTSUM1; //Cultivar specific temperature sum to reach anthesis
	
	FWSimVariable<Double> cReductionPerDHAboveTempCritical; //Find in literature; cultivar trait

  // Definition of Inputs
	FWSimVariable<Double> iTSUM; //Current temperature sum
	FWSimVariable<Double> iDVS; // crop development stage
	FWSimVariable<Double[]> Tinput; //Canopy temperature estimated from upper and lower limits and Ks
		
	FWSimVariable<Double> iYield; 
	
	//states
	FWSimVariable<Double> TSUMprevDay; //Temperature sum from previous day
	
	FWSimVariable<Double> sCumulativeDHAboveTempCritical;  //cumulative sCumulatedHeatStressFactor
	
	FWSimVariable<Double> sHSRedFactor; //reduces potential yield to account for kernel abortion

	// rates
	FWSimVariable<Double> rDailyDHAboveTempCritical;  //daily increment in sCumulatedHeatStressFactor
	
	
	// Definition of Outputs
	FWSimVariable<Double> HSAdjustedYield; //Storage organ mass adjusted for high temperatures near flowering
	
	FWSimVariable<Double> StartHSTSUM; //TSUM when determination of yield reduction due to heat stress started
	FWSimVariable<Double> FinalHSTSUM;  //TSUM when determination of yield reduction due to heat stress ended
	FWSimVariable<Integer> HSPeriodStartDOY; //DOY when determination of yield reduction due to heat stress started
	FWSimVariable<Integer> HSPeriodEndDOY; //DOY when determination of yield reduction due to heat stress ended
	FWSimVariable<Integer> NumHoursWithHS;  //Number of days when temperatures in sensitive period exceeded cTCritical 
	
	boolean sHSPeriodNotEnded;
	boolean sHSPeriodNotStarted;
	  

  /** @param aName
   * @param aFieldMap
   * @param aInputMap
   * @param aSimComponentElement
   * @param aVarMap
   * @param aOrderNumber */
  public HeatStressHourly(String aName, HashMap<String, FWSimVariable<?>> aFieldMap,
      HashMap<String, String> aInputMap, Element aSimComponentElement, FWSimVarMap aVarMap, int aOrderNumber)
  {
    super(aName, aFieldMap, aInputMap, aSimComponentElement, aVarMap, aOrderNumber);
  }

  /** Empty constructor used by class.forName() */
  public HeatStressHourly()
  {
    super();
  }

  /** @see net.simplace.simulation.model.FWSimComponent#createVariables() */
  @Override
  public HashMap<String, FWSimVariable<?>> createVariables()
  {

	  addVariable(FWSimVariable.createSimVariable("cTCritical","Temperature at which reduction in final yield occurs due to kernel abortion",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"C", null, null,32d, this));
	  addVariable(FWSimVariable.createSimVariable("cBeforeTSUM1","Start of sensitive period for kernel abortion relative to anthesis; 250DD before silking in maize",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"C days", null, null, 250.0, this));
	  addVariable(FWSimVariable.createSimVariable("cAfterTSUM1","End of sensitive period for kernel abortion relative to anthesis; 100DD after silking in maize",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"C day", null, null, 100.0, this));
	  addVariable(FWSimVariable.createSimVariable("cTSUM1","Cultivar specific temperature sum to reach anthesis",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"C day",null, null, null, this));
	  addVariable(FWSimVariable.createSimVariable("cReductionPerDHAboveTempCritical","reduction in kernel number/ yield per degree-hour above a threshold temp",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"one",null,null,0.05, this));
	  addVariable(FWSimVariable.createSimVariable("iTSUM","cumulative temperature sum from emergence",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"C day",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("iDVS","current crop development stage",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"one",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("iYield","storage organ yield",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"",null,null,null, this));
	  
	  addVariable(FWSimVariable.createSimVariable("TSUMprevDay","temperature sum from previous day",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"C day",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("iTinput","estimated canopy temperature",DATA_TYPE.DOUBLEARRAY, CONTENT_TYPE.input,"C",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("sCumulativeDHAboveTempCritical","cumulative degree-hours around flowering above cTCritical",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"C day",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("sHSRedFactor","yield reduction factor due to cumulative high temperatures above CTCritical",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"one",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("rDailyDHAboveTempCritical","todays increment in degree-hours above cTCritical",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"C day",null,null,null, this));
	  
	  addVariable(FWSimVariable.createSimVariable("HSAdjustedYield","yield of storage organs adjusted for high temperatures around flowering",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"g m-2",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("StartHSTSUM","Temperature sum at which determination of grain reduction due to heat stress started",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("FinalHSTSUM","Temperature sum at which determination of grain reduction due to heat stress ended",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("HSPeriodStartDOY","DOY when determination of grain reduction due to heat stress started",DATA_TYPE.INT, CONTENT_TYPE.out,"",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("HSPeriodEndDOY","DOY when determination of grain reduction due to heat stress ended",DATA_TYPE.INT, CONTENT_TYPE.out,"",null,null,null, this));	  
	  addVariable(FWSimVariable.createSimVariable("NumHoursWithHS","Count of hours when temperature exceeded cTCritical",DATA_TYPE.INT, CONTENT_TYPE.out,"",null,null,null, this));
    return iFieldMap;
  }

  /** @see net.simplace.simulation.model.FWSimComponent#init() */
  @Override
  protected void init()
  {


	  
	  TSUMprevDay.setValue(0.0, this);
	  sCumulativeDHAboveTempCritical.setValue(0.0, this);
	  sHSRedFactor.setValue(1.0, this);
	  rDailyDHAboveTempCritical.setValue(0.0, this);
	  StartHSTSUM.setValue(0.0, this);
	  FinalHSTSUM.setValue(0.0, this);
	  HSPeriodStartDOY.setValue(-1, this);
	  HSPeriodEndDOY.setValue(-1, this);
	  NumHoursWithHS.setValue(0, this);	
	  
	  sHSPeriodNotEnded = true; 
	  sHSPeriodNotStarted = true;
	  

  }

  /** @see net.simplace.simulation.model.FWSimComponent#process() */
  @Override
  protected void process()
  {		  
	  int hour = 0;
	  //	  double DAY= (double)((FWSimVariable<Integer>)getVariable(FWSimVarMap.CURRENT_DOY)).getValue();

	  if(iTSUM.getValue()<=0 )
	  {
		  reset();
	  }


	  if(iTSUM.getValue() >= (cTSUM1.getValue() - cBeforeTSUM1.getValue()) && (sHSPeriodNotEnded))
	  {

		  if (iTSUM.getValue() >= (cTSUM1.getValue() + cAfterTSUM1.getValue())) 
		  {
			  sHSPeriodNotEnded = false;
			  FinalHSTSUM.setValue(TSUMprevDay.getValue(), this); 
			  HSPeriodEndDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue() - 1, this);
		  }
		  else 
		  {
			  if(sHSPeriodNotStarted)
			  {
				  sHSPeriodNotStarted = false;
				  StartHSTSUM.setValue(iTSUM.getValue(), this);
				  HSPeriodStartDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue(), this);				  
			  }
			  TSUMprevDay.setValue(iTSUM.getValue(), this);

			  for (hour = 0; hour < 24; hour++ ) 
			  {
				  rDailyDHAboveTempCritical.setValue(Tinput.getValue()[hour] - cTCritical.getValue(), this);

				  if(rDailyDHAboveTempCritical.getValue() < 0.0)
				  {
					  rDailyDHAboveTempCritical.setValue(0.0, this);
				  }
				  else
				  {
					  NumHoursWithHS.setValue(NumHoursWithHS.getValue() + 1, this);
				  }
				  sCumulativeDHAboveTempCritical.setValue(sCumulativeDHAboveTempCritical.getValue()+ rDailyDHAboveTempCritical.getValue(), this);
			  }

			  sHSRedFactor.setValue(1.0- cReductionPerDHAboveTempCritical.getValue()*sCumulativeDHAboveTempCritical.getValue(), this);
			  if(sHSRedFactor.getValue()<0.0) sHSRedFactor.setValue(0.0, this); 
			  //			  	HSAdjustedYield.setValue(sHSRedFactor.getValue()*iYield.getValue(), this);
		  }
	  }
	  else
	  {
		  if(!(iYield == null))
		  {
			  HSAdjustedYield.setValue(iYield.getValue(), this);
		  }
		  else
		  {
			  HSAdjustedYield.setValue(0.0, this);
		  }
	  }
	  //	  if(iTSUM.getValue() >= (cTSUM1.getValue() - cBeforeTSUM1.getValue()) && (!sHSPeriodNotEnded)) {
	  HSAdjustedYield.setValue(sHSRedFactor.getValue()*iYield.getValue(), this);
	  //	  }
  }
	  
  
  protected void reset()
  {
	  TSUMprevDay.setValue(0.0, this);
	  sCumulativeDHAboveTempCritical.setValue(0.0, this);
	  sHSRedFactor.setValue(1.0, this);
	  rDailyDHAboveTempCritical.setValue(0.0, this);
	  StartHSTSUM.setValue(0.0, this);
	  FinalHSTSUM.setValue(0.0, this);
	  HSPeriodStartDOY.setValue(-1, this);
	  HSPeriodEndDOY.setValue(-1, this);
	  NumHoursWithHS.setValue(0, this);	
	  
	  sHSPeriodNotEnded = true; 
	  sHSPeriodNotStarted = true;
	  
  }

 
  /** creates a clone from this SimComponent for use in other threads
   * 
   * @see net.simplace.simulation.model.FWSimComponent#clone(net.simplace.simulation.util.FWSimVarMap) */
  @Override
  protected FWSimComponent clone(FWSimVarMap aVarMap)
  {
    return new HeatStressHourly(iName, iFieldMap, iInputMap, iSimComponentElement, aVarMap, iOrderNumber);
  }
}
