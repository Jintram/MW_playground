
# countdown timer

import matplotlib.pyplot as plt

cm_to_inch = 1/2.54

output_dir = '/Users/m.wehrens/Desktop/countdown_clock/'

# create a plot with a square with a countdown clock
def countdown_clock(seconds_to_countdown):
    # create a plot with a square
    fig, ax = plt.subplots(1,1, figsize=(4*cm_to_inch,2*cm_to_inch))
    # Create a recteangle
    #ax.plot([0, 1], [0, 0], 'k-')
    #ax.plot([1, 1], [0, 1], 'k-')
    #ax.plot([1, 0], [1, 1], 'k-')
    #ax.plot([0, 0], [1, 0], 'k-')
    
    ax.axis('off')

    # create a countdown clock
    for i in range(seconds_to_countdown+1):
        
        mins = i // 60
        secs = i % 60
        ax.text(0.5, 0.5, f'{mins:02}:{secs:02}', ha='center', va='center', fontsize=36)
        #plot.pause(1)
        
        # plt.show()
        fig.savefig(output_dir+f'countdown_clock_{(seconds_to_countdown-i):03}.png')
        
        ax.clear()
        ax.axis('off')
        
countdown_clock(180)