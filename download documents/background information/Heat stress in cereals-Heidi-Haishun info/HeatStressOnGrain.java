/*
 * SIMPLACE - Scientific Impact assessment and Modeling PLattform for Advanced Crop and Ecosystem management
 *
 * This file is part of the SIMPLACE (before SMILEUtil) project.
 * 
 * SIMPLACE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * SIMPLACE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with SIMPLACE.  If not, see <http://www.gnu.org/licenses/>.
 *
 * HeatStressOnGrain.java
 *
 * Responsible developers: Gunther Krauss, Crop Science Group, Katzenburgweg 5, 53115 Bonn, Germany
 *                         Andreas Enders, Crop Science Group, Katzenburgweg 5, 53115 Bonn, Germany
 * Contact Information:    lapit@uni-bonn.de
 * More information on <http://www.simplace.net>
 */

package net.simplace.client.simulation.lap;


import static java.lang.StrictMath.*;

import java.time.LocalDateTime;
import java.util.HashMap;

import net.simplace.simulation.model.FWSimComponent;
import net.simplace.simulation.util.FWSimVarMap;
import net.simplace.simulation.util.FWSimVariable;
import net.simplace.simulation.util.FWSimVariable.CONTENT_TYPE;
import net.simplace.simulation.util.FWSimVariable.DATA_TYPE;

import org.jdom.Element;


/**  * WIKI_START
 * HeatStressOnGrain calculates the heat stress factor (HeatStressFactor) around anthesis (approx. 30 days around anthesis) based on the daily heat stress intensity.
 * The day which day temperature is above certain threshold TempCritical (e.g. 27 °C for winter wheat) count as heated day.
 * The highest daily heat stress intensity occur whenever day temperature is above limited temperature TempLimit (e.g. 40 °C for winter wheat).
 * All equations documented in Teixeira et al., 2013.   
 * 
 * The critical and limit temperatures can be customized, as well as the number of days before and after anthesis, when 
 * stress should be taken into account. 
 * 
 * Instead of providing the number of days, one can give the devstages when the heat stress
 * period starts and ends. The number of days are then calculated from phenology.
 * 
 * Daily inputs are: min and max temperatures, dev stage, temperature sum, anthesis date and yield.
 * 
 * === Daily stress factor ===
 * WIKI_END
 \(
 
 \begin{eqnarray}
 TDay_i & = & Tmax_i - \frac{Tmax_i-Tmin_i}{4} \\
 
 DailyStressFactor_i & = & \max(0,\min(1,\frac{TDay_i - TempCritical}{TempLimit-TempCritical})) \\
 \end{eqnarray}
 \)
 * WIKI_START
 * === Heat stress factor ===
 * WIKI_END

 \(
 \begin{eqnarray}
 CummulatedHeatStressFactor & = & \sum_{i=AnthesisDOY - DaysBeforeAnthesis}^{AnthesisDoy + DaysFromAnthesis} DailyStressFactor_i \\
 
 HeatStressFactor & = & \frac{CummulatedHeatStressFactor}{DaysBeforeAnthesis + DaysFromAnthesis+1} \\
  \end{eqnarray}
 \)
  * WIKI_START
 * === Adjusted yield ===
 * WIKI_END

 \(
 \begin{eqnarray}
 AdjustedYield & = & (1-HeatStressFactor) \cdot Yield \\
 \end{eqnarray}
 \)
 * WIKI_START 
 * 
 * WIKI_END
 * WIKI_START
 *   '''References:'''
 * Teixeira, E., Fischer, G., Velthuizen, H., Walter, C., Ewert, F. 2013. 
 * Global hot-spots of heat stress on agricultural crops due to climate change.
 * Agriculture and Forest meteorology, 170:206-2015. 
 * WIKI_END
 * @author Gunther Krauss
 * @author Ehsan Eyshi Rezaei "eeyshire@uni-bonn.de"
 */
public class HeatStressOnGrain extends FWSimComponent
{

  // Definition of constants
	FWSimVariable<Double> cTempCritical;
	FWSimVariable<Double> cTempLimit;
	
	FWSimVariable<Integer> cDaysBeforeAnthesis;
	FWSimVariable<Integer> cDaysFromAnthesis;
	
	FWSimVariable<Double> cBeginDevStage;
	FWSimVariable<Double> cEndDevStage;

  // Definition of Inputs
	FWSimVariable<Double> iTMax;
	FWSimVariable<Double> iTMin;
	FWSimVariable<Double> iDevStage;
	FWSimVariable<Double> iTempSum;
	FWSimVariable<LocalDateTime> iAnthesisDate;
	
	FWSimVariable<Double> iYield;
	
	//states
	FWSimVariable<Double> sTDay;
	FWSimVariable<Double> sCumulatedHeatStressFactor;
	FWSimVariable<Integer> sDays;
	FWSimVariable<Integer> sAffectedDays;
	
	// rates
	FWSimVariable<Double> rDailyHeatStressFactor;
	
	// Definition of Outputs
	FWSimVariable<Double> HeatStressFactor;
	FWSimVariable<Integer> BeginDOY;
	FWSimVariable<Integer> EndDOY;
	FWSimVariable<Double> BeginDevStage;
	FWSimVariable<Double> EndDevStage;
	FWSimVariable<Double> BeginTempSum;
	FWSimVariable<Double> EndTempSum;
	FWSimVariable<Double> AdjustedYield;
	
	double[] DailyStressFactors;
	double[] DailyDevStages;
	double[] DailyTempSums;
	boolean useCalendarDays = false;
	double actualfactor = 0;
	boolean periodEnded = false;
	
	int sensitivePeriod = 0;
	int daysbeforeanthesis = 0;
	int day = 0;
	boolean calculatedPreAnthesis = false;
  

  /** @param aName
   * @param aFieldMap
   * @param aInputMap
   * @param aSimComponentElement
   * @param aVarMap
   * @param aOrderNumber */
  public HeatStressOnGrain(String aName, HashMap<String, FWSimVariable<?>> aFieldMap,
      HashMap<String, String> aInputMap, Element aSimComponentElement, FWSimVarMap aVarMap, int aOrderNumber)
  {
    super(aName, aFieldMap, aInputMap, aSimComponentElement, aVarMap, aOrderNumber);
  }

  /** Empty constructor used by class.forName() */
  public HeatStressOnGrain()
  {
    super();
  }

  /** @see net.simplace.simulation.model.FWSimComponent#createVariables() */
  @Override
  public HashMap<String, FWSimVariable<?>> createVariables()
  {

	  addVariable(FWSimVariable.createSimVariable("cTempCritical","Critical temperature threshold to start of heat stress",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius",null,null,27d, this));
	  addVariable(FWSimVariable.createSimVariable("cTempLimit","The temperature which maximum heat stress occurs",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius",null,null,40d, this));
	  addVariable(FWSimVariable.createSimVariable("cDaysBeforeAnthesis","Thermal sensitive days before anthesis",DATA_TYPE.INT, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("cDaysFromAnthesis","Thermal sensitive days after anthesis",DATA_TYPE.INT, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("cBeginDevStage","Development stage before anthesis which thermal sensitive period start",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,.8, this));
	  addVariable(FWSimVariable.createSimVariable("cEndDevStage","Development stage before anthesis which thermal sensitive period end",DATA_TYPE.DOUBLE, CONTENT_TYPE.constant,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,1.3, this));
	  addVariable(FWSimVariable.createSimVariable("iTMax","Daily maximum temperature",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("iTMin","Daily minimum temperature",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("iDevStage","Development stage",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("iTempSum","Temperature sum",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius_day",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("iAnthesisDate","Anthesis date",DATA_TYPE.DATE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,null, this));
	  addVariable(FWSimVariable.createSimVariable("iYield","Simulated yield before heat stress adjustment",DATA_TYPE.DOUBLE, CONTENT_TYPE.input,"http://www.wurvoc.org/vocabularies/om-1.8/gram_per_square_metre",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("sTDay","Day time temperature",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("sCumulatedHeatStressFactor","Cumulative heat stress factor",DATA_TYPE.DOUBLE, CONTENT_TYPE.state,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("sDays","Number of days related to thermal sensitive period",DATA_TYPE.INT, CONTENT_TYPE.state,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("sAffectedDays","Days which heat stress occurs",DATA_TYPE.INT, CONTENT_TYPE.state,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("rDailyHeatStressFactor","Daily heat stress factor",DATA_TYPE.DOUBLE, CONTENT_TYPE.rate,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("HeatStressFactor","Heat stress factor",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("BeginDOY","Begin of thermal sensitive period (day of year format)",DATA_TYPE.INT, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("EndDOY","End of thermal sensitive period (day of year format)",DATA_TYPE.INT, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0, this));
	  addVariable(FWSimVariable.createSimVariable("BeginDevStage","Begin of thermal sensitive period (Development stage format)",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("EndDevStage","End of thermal sensitive period (Development stage format)",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/one",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("BeginTempSum","Begin of thermal sensitive period (temperature sum format)",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius_day",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("EndTempSum","End of thermal sensitive period (temperature sum format)",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/degree_Celsius_day",null,null,0d, this));
	  addVariable(FWSimVariable.createSimVariable("AdjustedYield","Adjusted yield by heat stress",DATA_TYPE.DOUBLE, CONTENT_TYPE.out,"http://www.wurvoc.org/vocabularies/om-1.8/gram_per_square_metre",null,null,0d, this));
    return iFieldMap;
  }

  /** @see net.simplace.simulation.model.FWSimComponent#init() */
  @Override
  protected void init()
  {
	  if(cDaysBeforeAnthesis.getValue()>0 && cDaysFromAnthesis.getValue()>0)
	  {
		  useCalendarDays = true;
		  DailyStressFactors = new double[cDaysBeforeAnthesis.getValue()];
		  DailyDevStages = new double[cDaysBeforeAnthesis.getValue()];
		  DailyTempSums = new double[cDaysBeforeAnthesis.getValue()];
          calculatedPreAnthesis = false;
          for(int i = 0; i<cDaysBeforeAnthesis.getValue();i++)
          {
        	  DailyDevStages[i]=0;
        	  DailyStressFactors[i]=0;
        	  DailyTempSums[i]=0;
          }
		  
	  }
	  
	  actualfactor = 0;
	  periodEnded = false;
	  day = 0;
  }

  /** @see net.simplace.simulation.model.FWSimComponent#process() */
 	@Override
  protected void process()
  {
	  if(iDevStage.getValue()<=0 && EndDevStage.getValue()>0)
	  {
		  reset();
	  }

	  if(useCalendarDays)
	  {
		 
		  if(iAnthesisDate.getValue()==null)
		  {
			  
			  int mod = day % cDaysBeforeAnthesis.getValue();
			  day++;
			  
			  DailyDevStages[mod] = iDevStage.getValue();
			  DailyTempSums[mod] = iTempSum.getValue();
			  
			  sTDay.setValue(iTMax.getValue() - (iTMax.getValue()-iTMin.getValue())/4, this);
			  if(sTDay.getValue() < cTempCritical.getValue())
			  {
				  DailyStressFactors[mod] = 0d;
			  }
			  else if (sTDay.getValue() > cTempLimit.getValue())
			  {
				  DailyStressFactors[mod] = 1d;
			  }
			  else
			  {
				  DailyStressFactors[mod]=(sTDay.getValue()-cTempCritical.getValue())/(cTempLimit.getValue()-cTempCritical.getValue());				 
			  }  

		  }
		  else
		  {
			  if(!calculatedPreAnthesis)
			  {
				  calculatedPreAnthesis = true;
				  BeginTempSum.setValue(DailyTempSums[0], this);
				  BeginDevStage.setValue(DailyDevStages[0], this);
				  for(int i=0; i<cDaysBeforeAnthesis.getValue(); i++)
				  {
					  sCumulatedHeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()+DailyStressFactors[i], this);
					  if(DailyStressFactors[i]>0)
					  {
						  sAffectedDays.setValue(sAffectedDays.getValue()+1, this);
					  }
					  BeginTempSum.setValue(min(BeginTempSum.getValue(), DailyTempSums[i]), this);
					  BeginDevStage.setValue(min(BeginDevStage.getValue(), DailyDevStages[i]), this);
				  }

				  BeginDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue()-cDaysBeforeAnthesis.getValue(), this);

				  sDays.setValue(cDaysBeforeAnthesis.getValue(), this);
				  day = 0;
			  }
			  if(day < cDaysFromAnthesis.getValue())
			  {
				  day++;
				  sDays.setValue(sDays.getValue()+1, this);
				  sTDay.setValue(iTMax.getValue() - (iTMax.getValue()-iTMin.getValue())/4, this);
				  if(sTDay.getValue() < cTempCritical.getValue())
				  {
					  rDailyHeatStressFactor.setValue(0d, this);
				  }
				  else if (sTDay.getValue() > cTempLimit.getValue())
				  {
					  rDailyHeatStressFactor.setValue(1d, this);
					  sAffectedDays.setValue(sAffectedDays.getValue()+1, this);
					  sCumulatedHeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()+rDailyHeatStressFactor.getValue(), this);
				  }
				  else
				  {
					  rDailyHeatStressFactor.setValue((sTDay.getValue()-cTempCritical.getValue())/(cTempLimit.getValue()-cTempCritical.getValue()), this);
					  sAffectedDays.setValue(sAffectedDays.getValue()+1, this);
					  sCumulatedHeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()+rDailyHeatStressFactor.getValue(), this);
				  }
				  EndDevStage.setValue(iDevStage.getValue(), this);

				  EndDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue(), this);
				  EndTempSum.setValue(iTempSum.getValue(), this);				  
			  }
			  else if(day==cDaysFromAnthesis.getValue())
			  {
				  sTDay.setDefaultValue();
				  HeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()/sDays.getValue(), this);
				  day++;
			  }
		  }
			  
	  }
	  
	  
	  
	  else
	  {

		  if(cBeginDevStage.getValue() <= iDevStage.getValue() && iDevStage.getValue()<=cEndDevStage.getValue())
		  {
			  if(sDays.getValue()==0)
			  {
				  BeginDevStage.setValue(iDevStage.getValue(), this);
				  BeginDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue(), this);
				  BeginTempSum.setValue(iTempSum.getValue(), this);
			  }
			  sDays.setValue(sDays.getValue()+1, this);
			  
			  sTDay.setValue(iTMax.getValue() - (iTMax.getValue()-iTMin.getValue())/4, this);
			  if(sTDay.getValue() < cTempCritical.getValue())
			  {
				  rDailyHeatStressFactor.setValue(0d, this);
			  }
			  else if (sTDay.getValue() > cTempLimit.getValue())
			  {
				  rDailyHeatStressFactor.setValue(1d, this);
				  sAffectedDays.setValue(sAffectedDays.getValue()+1, this);
				  sCumulatedHeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()+rDailyHeatStressFactor.getValue(), this);
			  }
			  else
			  {
				  rDailyHeatStressFactor.setValue((sTDay.getValue()-cTempCritical.getValue())/(cTempLimit.getValue()-cTempCritical.getValue()), this);
				  sAffectedDays.setValue(sAffectedDays.getValue()+1, this);
				  sCumulatedHeatStressFactor.setValue(sCumulatedHeatStressFactor.getValue()+rDailyHeatStressFactor.getValue(), this);
			  }
			  
			  EndDevStage.setValue(iDevStage.getValue(), this);
			  EndDOY.setValue((Integer)getVariable(FWSimVarMap.CURRENT_DOY).getValue(), this);

			  EndTempSum.setValue(iTempSum.getValue(), this);
			  actualfactor = sCumulatedHeatStressFactor.getValue()/sDays.getValue();
		  }
		  else if(!periodEnded && iDevStage.getValue()>cEndDevStage.getValue())
		  {
			  periodEnded = true;
			  HeatStressFactor.setValue(actualfactor, this);		
			  sTDay.setDefaultValue();
		  }		  
		  
	  }
	  
	  if(iYield.getValue()!=null && periodEnded)
	  {
		  AdjustedYield.setValue((1-HeatStressFactor.getValue())*iYield.getValue(), this);
	  }
	  else
	  {
		  AdjustedYield.setValue(0.0, this);
	  }

  }
  
  private void reset()
  {
	  BeginDevStage.setDefaultValue();
	  EndDevStage.setDefaultValue();
	  BeginDOY.setDefaultValue();
	  EndDOY.setDefaultValue();
	  BeginTempSum.setDefaultValue();
	  EndTempSum.setDefaultValue();
	  HeatStressFactor.setDefaultValue();
	  
	  sDays.setDefaultValue();
	  sTDay.setDefaultValue();
	  sAffectedDays.setDefaultValue();
	  sCumulatedHeatStressFactor.setDefaultValue();
	  rDailyHeatStressFactor.setDefaultValue();
	  
      calculatedPreAnthesis = false;
      for(int i = 0; i<cDaysBeforeAnthesis.getValue();i++)
      {
    	  DailyDevStages[i]=0;
    	  DailyStressFactors[i]=0;
    	  DailyTempSums[i]=0;
      }
      day = 0;
      periodEnded = false;

	  
  }

  

  /** creates a clone from this SimComponent for use in other threads
   * 
   * @see net.simplace.simulation.model.FWSimComponent#clone(net.simplace.simulation.util.FWSimVarMap) */
  @Override
  protected FWSimComponent clone(FWSimVarMap aVarMap)
  {
    return new HeatStressOnGrain(iName, iFieldMap, iInputMap, iSimComponentElement, aVarMap, iOrderNumber);
  }
}
