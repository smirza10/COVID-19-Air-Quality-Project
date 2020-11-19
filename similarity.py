import statistics
import math
from statistics import mean
def main():
          #Population Density Cases   Beds/100k    %below poverty
    cook= [[5150233, 5495.1,  139583, 369.3230966, 13.8],
           [10039107.00, 2419.60, 261446.00, 300.2956339, 14.20],
           [4713325.00, 2402.40, 137946.00, 341.4150308, 16.50],
           [4485414.00, 414.9, 140006.00, 222.0084924, 12.30],
           [3338330.00, 735.8, 44925.00, 253.0007519, 11.50],
           [3175692.00, 3807.70, 52201.00, 264.572257, 10.50],
            [2716940.00, 1315.00, 167153.00, 366.7729136, 16.00],
            [2635516.00, 2718.00, 78205.00,	364.2171021, 14.20],
            [2252782.00, 912.9, 21516.00, 261.1881665, 9.20],
            [2102515.00, 2094.70, 47231.00, 326.3710366, 12.10],
            [1952778.00, 1444.90, 75801.00, 354.0084946, 12.60],
            [1927852.00, 1381.00, 20511.00, 235.8064831, 7.30],
            [1749343.00, 2974.40, 34302.00,	348.7595057, 21.70],
            [1584064.00, 11379.50, 35825.00, 546.8213406, 24.30],
            [1496770.00, 670.2,	45425.00, 333.5849863, 12.20],
            [1063937.00, 1748.00, 27080.00, 493.6382511, 13.50],
            [945726.00,	3926.00, 26973.00, 518.9663814,	19.10]]
    #normalize the attributes
    cookNorm=zScore(cook)
    #Finding the distance between the records
    cookSim = distance(cookNorm)
    cookNames = ["Cook", "Los Angeles", "Harris", "Maricopa", "San Diego","Orange", "Miami-Dade", "Dallas", "King", "Tarrant", "Broward", "Santa Clara", "Wayne", "Philadelphia", "Palm Beach", "Fulton", "Milwaukee"]
    printScore(cookSim, cookNames)
    kane = [[532403.00, 990.8, 12657.00, 206.7982337, 8.60],
            [194672.00, 227.4, 2246, 536.8003616, 13.10],
            [179179.00, 301.2, 3225, 626.189453, 16.60],
            [887207.00,	754.3, 11607.00, 178.7632424, 7.30],
            [846006.00, 446.7, 12406.00, 191.9608135, 9.10],
            [636235.00,	257.8, 15828.00, 213.1287967, 9.40],	
            [537174.00, 756.4, 8157.00, 341.2302159, 15.40],
            [374264.00,	230.7, 11047.00, 106.0748563, 10.10],
            [342139.00, 769.9, 11450.00, 207.8102759, 12.10],
            [332285.00,	424, 9031.00, 52.0637, 10.40],
            [324492.00, 63.4, 4540.00, 412.0286479, 10.50],
            [271826.00,	583, 6109.00, 298.3526226, 13.50],
            [269043.00, 282.7, 7581.00, 656.0289619, 19.80],
            [244390.00,	824.6, 8822.00,	508.2041, 15.30],
            [234473.00, 1058.10, 3914, 327.9695317, 15.3],
            [232751.00,	227.6, 3620, 574.4336222, 15],
            [229211.00, 332.8, 6102.00, 300.1601145, 23.20],
            [209674.00, 476.1, 2401.00,	526.5316634, 14.20],
            [181451.00, 769.7, 3383, 713.1401866, 15.10], 
            [176875.00,	327.1, 2577, 545.0176678, 13.90],
            [168424.00, 66.7, 1041, 1009.357336, 17.20],
            [166223.00,	152.8, 4649, 611.2270865, 12.90],
            [158293.00, 220.8, 2319, 1339.288535, 7.10],
            [122259.00,	243.1, 2087, 124.3262255, 17.00]]
    #Coping the stats for peoria county  and sangamon     
    peoria = kane.copy()
    sangamon = kane.copy()
    kaneNorm = zScore(kane) 
    kaneSim = distance(kaneNorm)       
    kaneNames = ["Kane","Sangamon","Peoria", "Denton","Ventura", "Utah", "Guilford","Brazoria", "Galveston","Rutherford", "Weld", "St. Joseph","Alachua", "Lafayette", "New Hanover", "Smith", "Brazos", "Lackawanna", "Vanderburgh", "Shawnee", "Pueblo","Ector", "Olmsted","Douglas"]
    #Coping the names for peoria and sangamon
    peoriaNames = kaneNames.copy()
    sangamonNames = kaneNames.copy()
    printScore(kaneSim, kaneNames)
    #Swithing values so peoria is first
    temp = peoria[0]
    peoria[0] = peoria[2]
    peoria[2] = temp
    temp = peoriaNames[0]
    peoriaNames[0] = peoriaNames[2]
    peoriaNames[2] = temp
    peoriaNorm = zScore(peoria)
    peoriaSim = distance(peoriaNorm) 
    printScore(peoriaSim, peoriaNames)
    temp = sangamon[0]
    sangamon[0] = sangamon[1]
    sangamon[1] = temp
    temp = sangamonNames[0]
    sangamonNames[0] = sangamonNames[1]
    sangamonNames[1] = temp
    sangamonNorm = zScore(sangamon)
    sangamonSim = distance(sangamonNorm) 
    printScore(sangamonSim, sangamonNames)

#Zscore Normalization method    
def zScore(arr):
    r = len(arr)
    c = len(arr[0])
    pop = [0] * (r)
    den = [0] * (r)
    cases = [0] * (r)
    beds = [0] * (r)
    poverty = [0] * (r)
    avg = [0,0,0,0,0]
    for i in range(r):
        pop[i] = arr[i][0]  #adding up values for each row
        den[i] = arr[i][1]
        cases[i] = arr[i][2]
        beds[i] = arr[i][3] 
        poverty[i] = arr[i][4]    
    avg[0] = mean(pop) # using mean method to find average
    avg[1] = mean(den)
    avg[2] = mean(cases)
    avg[3] = mean(beds)
    avg[4] = mean(poverty)
    stdPop = statistics.stdev(pop)
    stdDen= statistics.stdev(den) #finding the standard deviation
    stdCases = statistics.stdev(cases)
    stdBeds = statistics.stdev(beds)
    stdPov = statistics.stdev(poverty)
    stdev = [stdPop,stdDen,stdCases,stdBeds, stdPov] # Creating an array with standard deviation for each column
    arrNorm = arr
    for i in range(r):
        for j in range(c):
            arrNorm[i][j] = (arr[i][j] - avg[j]) / stdev[j]  # zscore normalization of every value
    return(arrNorm)

def distance(arr): # Methond to get the distance
    r = len(arr)
    c = len(arr[0])
    sim = [0] * r #array to hold value of similarity scores
    for i in range(r):
        ed = 0
        for j in range(c):
            ed = ed +  (arr[0][j] - arr[i][j]) * (arr[0][j] - arr[i][j])
        ed = math.sqrt(ed)  #getting euclidiean distance for every row compared to the first one
        sim[i] = ed 
    return(sim)   
def printScore(sim, names): #print method for each row 
    r = len(sim) 
    for i in range(r):
        for j in range(r - 1):
            if(sim[j] > sim[j + 1]):  #bubble sorting the values based on similarity score
                temp = sim[j]
                sim[j]  = sim[j + 1]
                sim[j + 1 ] = temp
                temp = names[j]   
                names[j]  = names[j + 1]
                names[j + 1 ] = temp   
    for i in range(r):
        if(i != 0):
            print("Similarity Score between ", names[0], " and ", names[i], " is ", sim[i] ) #print statement
    print()            
    return         
main() 