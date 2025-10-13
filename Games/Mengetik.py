import pygame
import sys
import random
import time
import math
import json
from enum import Enum
from collections import deque

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 1200
WINDOW_HEIGHT = 800
FPS = 60

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GRAY = (128, 128, 128)
LIGHT_GRAY = (200, 200, 200)
DARK_GRAY = (64, 64, 64)
GREEN = (100, 255, 100)
RED = (255, 100, 100)
BLUE = (100, 150, 255)
YELLOW = (255, 255, 100)
PURPLE = (200, 100, 255)
ORANGE = (255, 165, 0)
CYAN = (100, 255, 255)

class GameState(Enum):
    MENU = "menu"
    DIFFICULTY_SELECT = "difficulty_select"
    TYPING_TEST = "typing_test"
    RESULTS = "results"
    STATISTICS = "statistics"

class Difficulty(Enum):
    BEGINNER = ("Beginner", "Simple words and phrases", 30)
    INTERMEDIATE = ("Intermediate", "Mixed sentences", 60)
    ADVANCED = ("Advanced", "Complex paragraphs", 90)
    EXPERT = ("Expert", "Technical content", 120)

class Particle:
    def __init__(self, x, y, color, velocity, life=60):
        self.x = x
        self.y = y
        self.color = color
        self.velocity = velocity
        self.life = life
        self.max_life = life
        self.size = random.randint(2, 6)
        self.spin = random.uniform(0, 2 * math.pi)
        self.spin_speed = random.uniform(-0.3, 0.3)
        
    def update(self):
        self.x += self.velocity[0]
        self.y += self.velocity[1]
        self.velocity = (self.velocity[0] * 0.98, self.velocity[1] * 0.98)
        self.life -= 1
        self.spin += self.spin_speed
        
    def draw(self, screen):
        if self.life > 0:
            alpha = int(255 * (self.life / self.max_life))
            size = max(1, int(self.size * (self.life / self.max_life)))
            
            # Create star-like particle
            points = []
            for i in range(6):
                angle = self.spin + i * math.pi / 3
                px = self.x + size * math.cos(angle)
                py = self.y + size * math.sin(angle)
                points.append((px, py))
            
            if len(points) >= 3:
                surface = pygame.Surface((size * 4, size * 4), pygame.SRCALPHA)
                color_with_alpha = (*self.color, alpha)
                adjusted_points = [(p[0] - self.x + size * 2, p[1] - self.y + size * 2) for p in points]
                pygame.draw.polygon(surface, color_with_alpha, adjusted_points)
                screen.blit(surface, (self.x - size * 2, self.y - size * 2))

class SkeletalCreature:
    """Animated skeletal creature inspired by the image"""
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.nodes = []
        self.segments = 25
        self.segment_length = 12
        self.target_x = x
        self.target_y = y
        self.animation_speed = 0.1
        
        # Initialize spine nodes
        for i in range(self.segments):
            node_x = x
            node_y = y + i * self.segment_length
            self.nodes.append({
                'x': node_x, 
                'y': node_y, 
                'angle': 0,
                'size': max(2, 8 - i // 4)
            })
    
    def moveTo(self, x, y):
        """Smooth movement towards target with realistic following"""
        self.target_x = x
        self.target_y = y
        
        if not self.nodes:
            return
        
        # Calculate distance and move head
        dx = x - self.nodes[0]['x']
        dy = y - self.nodes[0]['y']
        dist = math.sqrt(dx**2 + dy**2)
        
        if dist > 5:
            # Smooth head movement
            move_speed = min(2.5, dist * self.animation_speed)
            angle = math.atan2(dy, dx)
            self.nodes[0]['x'] += move_speed * math.cos(angle)
            self.nodes[0]['y'] += move_speed * math.sin(angle)
        
        # Update following segments with improved physics
        for i in range(1, len(self.nodes)):
            prev_node = self.nodes[i-1]
            current_node = self.nodes[i]
            
            dx = prev_node['x'] - current_node['x']
            dy = prev_node['y'] - current_node['y']
            distance = math.sqrt(dx**2 + dy**2)
            
            if distance > self.segment_length:
                # Calculate new position
                angle = math.atan2(dy, dx)
                current_node['x'] = prev_node['x'] - self.segment_length * math.cos(angle)
                current_node['y'] = prev_node['y'] - self.segment_length * math.sin(angle)
                current_node['angle'] = angle
    
    def update(self, typing_speed_factor=1.0):
        """Update creature animation based on typing speed"""
        # Make creature more active when typing faster
        self.animation_speed = 0.05 + typing_speed_factor * 0.15
        
        # Add subtle breathing/floating motion
        time_factor = time.time() * 2
        float_y = self.target_y + math.sin(time_factor) * 5
        self.moveTo(self.target_x, float_y)
    
    def draw(self, screen):
        if not self.nodes:
            return
        
        # Draw spine connections
        for i in range(len(self.nodes) - 1):
            start = (int(self.nodes[i]['x']), int(self.nodes[i]['y']))
            end = (int(self.nodes[i+1]['x']), int(self.nodes[i+1]['y']))
            
            # Fade effect along spine
            alpha = int(255 * (1 - i / len(self.nodes) * 0.7))
            
            # Draw glowing spine
            for thickness in range(5, 0, -1):
                spine_alpha = alpha // thickness
                color = (*WHITE, spine_alpha)
                
                spine_surface = pygame.Surface((abs(end[0] - start[0]) + thickness * 2, 
                                               abs(end[1] - start[1]) + thickness * 2), pygame.SRCALPHA)
                pygame.draw.line(spine_surface, color, 
                               (thickness, thickness), 
                               (end[0] - start[0] + thickness, end[1] - start[1] + thickness), thickness)
                screen.blit(spine_surface, (min(start[0], end[0]) - thickness, min(start[1], end[1]) - thickness))
        
        # Draw ribs with improved spacing
        for i, node in enumerate(self.nodes):
            if i % 2 == 0 and i > 0 and i < len(self.nodes) - 5:  # Skip head and tail
                rib_length = max(8, 25 - i * 0.8)
                angle = node['angle'] + math.pi / 2
                
                # Add slight curve to ribs
                curve_offset = math.sin(i * 0.3) * 3
                
                # Left rib
                rib_end_x = node['x'] + rib_length * math.cos(angle) + curve_offset
                rib_end_y = node['y'] + rib_length * math.sin(angle)
                self.draw_rib(screen, node, rib_end_x, rib_end_y, i)
                
                # Right rib
                rib_end_x = node['x'] - rib_length * math.cos(angle) + curve_offset
                rib_end_y = node['y'] - rib_length * math.sin(angle)
                self.draw_rib(screen, node, rib_end_x, rib_end_y, i)
        
        # Draw vertebrae (nodes)
        for i, node in enumerate(self.nodes):
            alpha = int(255 * (1 - i / len(self.nodes) * 0.5))
            size = node['size']
            
            # Main vertebra
            vertebra_surface = pygame.Surface((size * 3, size * 3), pygame.SRCALPHA)
            color = (*WHITE, alpha)
            pygame.draw.circle(vertebra_surface, color, (size * 1.5, size * 1.5), size)
            screen.blit(vertebra_surface, (node['x'] - size * 1.5, node['y'] - size * 1.5))
            
            # Highlight
            if size > 2:
                highlight_surface = pygame.Surface((size, size), pygame.SRCALPHA)
                highlight_color = (*WHITE, alpha // 2)
                pygame.draw.circle(highlight_surface, highlight_color, (size // 2, size // 2), size // 3)
                screen.blit(highlight_surface, (node['x'] - size // 2, node['y'] - size // 2))
    
    def draw_rib(self, screen, node, end_x, end_y, index):
        """Draw individual rib with glow effect"""
        start = (int(node['x']), int(node['y']))
        end = (int(end_x), int(end_y))
        
        alpha = int(200 * (1 - index / len(self.nodes)))
        
        # Draw rib with multiple passes for glow
        for thickness in range(3, 0, -1):
            rib_alpha = alpha // thickness
            color = (*WHITE, rib_alpha)
            
            rib_surface = pygame.Surface((abs(end[0] - start[0]) + thickness * 2,
                                        abs(end[1] - start[1]) + thickness * 2), pygame.SRCALPHA)
            pygame.draw.line(rib_surface, color,
                           (thickness, thickness),
                           (end[0] - start[0] + thickness, end[1] - start[1] + thickness), thickness)
            screen.blit(rib_surface, (min(start[0], end[0]) - thickness, min(start[1], end[1]) - thickness))

class TypingSpeedTest:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("⌨️ Typing Speed Test - Ultimate Edition")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.font_title = pygame.font.Font(None, 64)
        self.font_large = pygame.font.Font(None, 48)
        self.font_medium = pygame.font.Font(None, 36)
        self.font_small = pygame.font.Font(None, 28)
        self.font_tiny = pygame.font.Font(None, 20)
        
        # Game state
        self.game_state = GameState.MENU
        self.current_difficulty = Difficulty.BEGINNER
        self.menu_selection = 0
        
        # Typing test variables
        self.test_text = ""
        self.user_input = ""
        self.current_position = 0
        self.start_time = 0
        self.end_time = 0
        self.is_started = False
        self.is_finished = False
        
        # Statistics
        self.wpm = 0
        self.accuracy = 100.0
        self.errors = 0
        self.total_characters = 0
        self.correct_characters = 0
        
        # Visual effects
        self.particles = []
        self.background_particles = []
        self.creature = SkeletalCreature(WINDOW_WIDTH - 200, WINDOW_HEIGHT // 2)
        self.typing_intensity = 0
        
        # Text samples
        self.text_samples = {
            Difficulty.BEGINNER: [
                "The quick brown fox jumps over the lazy dog.",
                "A journey of a thousand miles begins with a single step.",
                "Practice makes perfect when you type every day.",
                "Simple words help build confidence and speed.",
                "Keep calm and type on with steady hands."
            ],
            Difficulty.INTERMEDIATE: [
                "Programming is the art of telling another human being what one wants the computer to do. The computer itself is just a tool, like a very fast pencil.",
                "Success is not final, failure is not fatal: it is the courage to continue that counts. Winston Churchill once said that persistence is the key to achievement.",
                "Technology has revolutionized the way we communicate, learn, and work in the modern world. From smartphones to artificial intelligence, innovation continues to shape our future.",
                "The beauty of programming lies not just in solving problems, but in crafting elegant solutions that are both efficient and maintainable for future developers.",
                "Time management is crucial for productivity. By organizing tasks and setting priorities, we can accomplish more while maintaining a healthy work-life balance."
            ],
            Difficulty.ADVANCED: [
                "In computer science, algorithmic complexity analysis involves examining how the runtime or space requirements of an algorithm scale with input size. Big O notation provides a mathematical framework for describing these growth patterns, helping developers optimize their code for better performance.",
                "Machine learning algorithms can be broadly categorized into supervised, unsupervised, and reinforcement learning paradigms. Each approach has distinct characteristics: supervised learning uses labeled training data, unsupervised learning finds patterns in unlabeled data, and reinforcement learning learns through interaction with an environment.",
                "The implementation of distributed systems presents unique challenges including network latency, partial failures, and data consistency. CAP theorem demonstrates that it's impossible to simultaneously guarantee consistency, availability, and partition tolerance in a distributed database system.",
                "Quantum computing leverages quantum mechanical phenomena such as superposition and entanglement to process information in fundamentally different ways than classical computers. Quantum algorithms like Shor's algorithm for factoring large integers could potentially break current cryptographic systems.",
                "Software engineering principles emphasize the importance of clean code, proper documentation, and comprehensive testing. Design patterns provide reusable solutions to common programming problems, while version control systems enable collaborative development and change tracking."
            ],
            Difficulty.EXPERT: [
                "The paradigm shift towards microservices architecture has fundamentally altered how we approach system design and deployment. By decomposing monolithic applications into smaller, independently deployable services, organizations can achieve greater scalability, resilience, and team autonomy. However, this architectural pattern introduces complexity in service discovery, inter-service communication, and distributed transaction management.",
                "Cryptocurrency blockchain technology utilizes cryptographic hash functions and distributed consensus mechanisms to maintain immutable transaction ledgers without requiring trusted intermediaries. Proof-of-work algorithms ensure network security through computational difficulty, while smart contracts enable programmable, self-executing agreements with predetermined conditions and automated enforcement.",
                "Advanced compiler optimization techniques including loop unrolling, constant propagation, and dead code elimination significantly impact program performance. Static single assignment form facilitates dataflow analysis, enabling transformations such as common subexpression elimination and register allocation through sophisticated graph coloring algorithms.",
                "Neuromorphic computing architectures attempt to emulate biological neural networks by implementing spiking neural networks in specialized hardware. These event-driven systems promise energy-efficient computation for artificial intelligence applications, particularly in pattern recognition and sensory processing tasks that traditional von Neumann architectures handle inefficiently.",
                "Distributed consensus protocols like Raft and Byzantine Fault Tolerance ensure data consistency across replicated systems even in the presence of network partitions and malicious actors. These algorithms coordinate state machine replication through leader election, log replication, and safety guarantees that preserve linearizability under various failure scenarios."
            ]
        }
        
        # Initialize background particles
        self.init_background_particles()
        
        # Load statistics
        self.statistics = self.load_statistics()
        
        # Sound system
        self.sound_enabled = True
        self.init_sounds()
    
    def init_sounds(self):
        """Initialize sound effects"""
        try:
            pygame.mixer.init(frequency=22050, size=-16, channels=2, buffer=512)
            self.sound_enabled = True
        except:
            self.sound_enabled = False
    
    def play_sound(self, sound_type):
        """Play typing sound effects"""
        if not self.sound_enabled:
            return
        
        try:
            if sound_type == "key_correct":
                # Pleasant key sound
                duration = 0.05
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 800 * math.sin(2 * math.pi * 800 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "key_error":
                # Error sound
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 1200 * math.sin(2 * math.pi * 200 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "finish":
                # Completion sound
                duration = 0.8
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq1 = 523 + (i * 200 / frames)  # C to D
                    freq2 = 659 + (i * 200 / frames)  # E to F#
                    wave1 = 600 * math.sin(2 * math.pi * freq1 * i / sample_rate)
                    wave2 = 600 * math.sin(2 * math.pi * freq2 * i / sample_rate)
                    wave = int((wave1 + wave2) * (1 - i / frames))
                    arr.append([wave, wave])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def init_background_particles(self):
        """Initialize background particle system"""
        for _ in range(30):
            x = random.randint(0, WINDOW_WIDTH)
            y = random.randint(0, WINDOW_HEIGHT)
            velocity = (random.uniform(-0.5, 0.5), random.uniform(-0.5, 0.5))
            color = random.choice([DARK_GRAY, GRAY])
            self.background_particles.append(Particle(x, y, color, velocity, life=2000))
    
    def load_statistics(self):
        """Load statistics from file"""
        try:
            with open('typing_stats.json', 'r') as f:
                return json.load(f)
        except:
            return {
                'games_played': 0,
                'best_wpm': 0,
                'best_accuracy': 0,
                'total_words_typed': 0,
                'average_wpm': 0,
                'difficulty_stats': {diff.name: {'games': 0, 'best_wpm': 0} for diff in Difficulty}
            }
    
    def save_statistics(self):
        """Save statistics to file"""
        try:
            with open('typing_stats.json', 'w') as f:
                json.dump(self.statistics, f, indent=2)
        except:
            pass
    
    def update_statistics(self):
        """Update statistics after completing a test"""
        self.statistics['games_played'] += 1
        self.statistics['best_wpm'] = max(self.statistics['best_wpm'], self.wpm)
        self.statistics['best_accuracy'] = max(self.statistics['best_accuracy'], self.accuracy)
        
        word_count = len(self.test_text.split())
        self.statistics['total_words_typed'] += word_count
        
        # Update average WPM
        if self.statistics['games_played'] > 0:
            total_wpm = self.statistics['average_wpm'] * (self.statistics['games_played'] - 1) + self.wpm
            self.statistics['average_wpm'] = total_wpm / self.statistics['games_played']
        
        # Update difficulty-specific stats
        diff_name = self.current_difficulty.name
        self.statistics['difficulty_stats'][diff_name]['games'] += 1
        self.statistics['difficulty_stats'][diff_name]['best_wpm'] = max(
            self.statistics['difficulty_stats'][diff_name]['best_wpm'], self.wpm
        )
        
        self.save_statistics()
    
    def start_typing_test(self):
        """Initialize a new typing test"""
        self.test_text = random.choice(self.text_samples[self.current_difficulty])
        self.user_input = ""
        self.current_position = 0
        self.start_time = 0
        self.end_time = 0
        self.is_started = False
        self.is_finished = False
        self.errors = 0
        self.total_characters = 0
        self.correct_characters = 0
        self.particles = []
        self.game_state = GameState.TYPING_TEST
    
    def process_keystroke(self, key):
        """Process user keystrokes during typing test"""
        if not self.is_started:
            self.is_started = True
            self.start_time = time.time()
        
        if key == '\b':  # Backspace
            if self.user_input:
                self.user_input = self.user_input[:-1]
                self.current_position = len(self.user_input)
        elif key == '\r' or key == '\n':  # Enter
            key = ' '  # Treat enter as space
            self.process_character(key)
        elif len(key) == 1 and ord(key) >= 32:  # Printable characters
            self.process_character(key)
    
    def process_character(self, char):
        """Process a single character input"""
        if self.current_position < len(self.test_text):
            expected_char = self.test_text[self.current_position]
            
            self.user_input += char
            self.total_characters += 1
            
            if char == expected_char:
                self.correct_characters += 1
                self.play_sound("key_correct")
                # Create success particles
                self.create_typing_particles(GREEN)
                self.typing_intensity = min(1.0, self.typing_intensity + 0.1)
            else:
                self.errors += 1
                self.play_sound("key_error")
                # Create error particles
                self.create_typing_particles(RED)
            
            self.current_position += 1
            
            # Check if test is complete
            if self.current_position >= len(self.test_text):
                self.finish_test()
    
    def create_typing_particles(self, color):
        """Create particles when typing"""
        # Create particles near the typing area
        for _ in range(5):
            x = WINDOW_WIDTH // 2 + random.randint(-100, 100)
            y = WINDOW_HEIGHT // 2 + random.randint(-50, 50)
            velocity = (random.uniform(-3, 3), random.uniform(-5, -1))
            self.particles.append(Particle(x, y, color, velocity, life=40))
    
    def finish_test(self):
        """Complete the typing test and calculate results"""
        self.is_finished = True
        self.end_time = time.time()
        
        # Calculate WPM (Words Per Minute)
        time_elapsed = self.end_time - self.start_time
        word_count = len(self.test_text.split())
        self.wpm = int((word_count / time_elapsed) * 60) if time_elapsed > 0 else 0
        
        # Calculate accuracy
        self.accuracy = (self.correct_characters / self.total_characters * 100) if self.total_characters > 0 else 100.0
        
        # Update statistics
        self.update_statistics()
        
        # Create celebration particles
        for _ in range(30):
            x = random.randint(0, WINDOW_WIDTH)
            y = random.randint(0, WINDOW_HEIGHT)
            velocity = (random.uniform(-5, 5), random.uniform(-8, -2))
            color = random.choice([GREEN, YELLOW, CYAN, PURPLE])
            self.particles.append(Particle(x, y, color, velocity, life=100))
        
        self.play_sound("finish")
        
        # Transition to results after a short delay
        pygame.time.set_timer(pygame.USEREVENT + 1, 2000)  # 2 seconds
    
    def calculate_real_time_wpm(self):
        """Calculate current WPM in real-time"""
        if not self.is_started or self.start_time == 0:
            return 0
        
        current_time = time.time()
        time_elapsed = current_time - self.start_time
        
        if time_elapsed == 0:
            return 0
        
        # Count words typed so far
        words_typed = len(self.user_input.split())
        return int((words_typed / time_elapsed) * 60)
    
    def get_typing_speed_factor(self):
        """Get typing speed factor for creature animation"""
        current_wpm = self.calculate_real_time_wpm()
        return min(1.0, current_wpm / 100.0)  # Normalize to 0-1 range
    
    def update(self):
        """Update game state"""
        # Update particles
        self.particles = [p for p in self.particles if p.life > 0]
        for particle in self.particles:
            particle.update()
        
        # Update background particles
        for particle in self.background_particles:
            particle.update()
            # Respawn background particles
            if particle.life <= 0:
                particle.x = random.randint(0, WINDOW_WIDTH)
                particle.y = random.randint(0, WINDOW_HEIGHT)
                particle.velocity = (random.uniform(-0.5, 0.5), random.uniform(-0.5, 0.5))
                particle.life = 2000
        
        # Update creature
        if self.game_state == GameState.TYPING_TEST:
            typing_factor = self.get_typing_speed_factor()
            self.creature.update(typing_factor)
        else:
            self.creature.update(0.2)
        
        # Decay typing intensity
        self.typing_intensity *= 0.95
    
    def handle_events(self):
        """Handle game events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            elif event.type == pygame.USEREVENT + 1:
                # Transition to results screen
                self.game_state = GameState.RESULTS
                pygame.time.set_timer(pygame.USEREVENT + 1, 0)  # Cancel timer
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    if self.game_state in [GameState.DIFFICULTY_SELECT, GameState.RESULTS, GameState.STATISTICS]:
                        self.game_state = GameState.MENU
                    elif self.game_state == GameState.TYPING_TEST:
                        self.game_state = GameState.MENU
                    elif self.game_state == GameState.MENU:
                        return False
                
                elif self.game_state == GameState.MENU:
                    if event.key == pygame.K_UP:
                        self.menu_selection = (self.menu_selection - 1) % 4
                    elif event.key == pygame.K_DOWN:
                        self.menu_selection = (self.menu_selection + 1) % 4
                    elif event.key == pygame.K_RETURN:
                        if self.menu_selection == 0:  # Start Test
                            self.game_state = GameState.DIFFICULTY_SELECT
                        elif self.menu_selection == 1:  # Statistics
                            self.game_state = GameState.STATISTICS
                        elif self.menu_selection == 2:  # Settings
                            pass  # Could add settings menu
                        elif self.menu_selection == 3:  # Exit
                            return False
                
                elif self.game_state == GameState.DIFFICULTY_SELECT:
                    if event.key == pygame.K_UP:
                        difficulties = list(Difficulty)
                        current_index = difficulties.index(self.current_difficulty)
                        self.current_difficulty = difficulties[(current_index - 1) % len(difficulties)]
                    elif event.key == pygame.K_DOWN:
                        difficulties = list(Difficulty)
                        current_index = difficulties.index(self.current_difficulty)
                        self.current_difficulty = difficulties[(current_index + 1) % len(difficulties)]
                    elif event.key == pygame.K_RETURN:
                        self.start_typing_test()
                
                elif self.game_state == GameState.TYPING_TEST and not self.is_finished:
                    if event.key == pygame.K_BACKSPACE:
                        self.process_keystroke('\b')
                    elif event.key == pygame.K_RETURN:
                        self.process_keystroke('\r')
                    else:
                        # Handle regular character input
                        char = event.unicode
                        if char:
                            self.process_keystroke(char)
                
                elif self.game_state == GameState.RESULTS:
                    if event.key == pygame.K_RETURN:
                        self.game_state = GameState.MENU
                    elif event.key == pygame.K_SPACE:
                        # Restart with same difficulty
                        self.start_typing_test()
        
        return True
    
    def draw_background(self):
        """Draw animated background"""
        # Gradient background
        for y in range(WINDOW_HEIGHT):
            color_factor = y / WINDOW_HEIGHT
            r = int(20 + 30 * color_factor)
            g = int(20 + 30 * color_factor)  
            b = int(30 + 40 * color_factor)
            pygame.draw.line(self.screen, (r, g, b), (0, y), (WINDOW_WIDTH, y))
        
        # Draw background particles
        for particle in self.background_particles:
            particle.draw(self.screen)
        
        # Draw creature
        self.creature.draw(self.screen)
    
    def draw_menu(self):
        """Draw main menu"""
        self.draw_background()
        
        # Animated title
        title_y = 150 + math.sin(time.time() * 2) * 10
        title_text = self.font_title.render("⌨️ TYPING SPEED TEST", True, WHITE)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        
        # Title glow effect
        for offset in range(3, 0, -1):
            glow_text = self.font_title.render("⌨️ TYPING SPEED TEST", True, CYAN)
            glow_rect = glow_text.get_rect(center=(WINDOW_WIDTH // 2 + offset, title_y + offset))
            self.screen.blit(glow_text, glow_rect)
        
        self.screen.blit(title_text, title_rect)
        
        # Subtitle
        subtitle = self.font_medium.render("Ultimate Edition", True, GRAY)
        subtitle_rect = subtitle.get_rect(center=(WINDOW_WIDTH // 2, title_y + 60))
        self.screen.blit(subtitle, subtitle_rect)
        
        # Menu options
        menu_options = [
            "🚀 Start Typing Test",
            "📊 View Statistics", 
            "⚙️ Settings",
            "🚪 Exit"
        ]
        
        for i, option in enumerate(menu_options):
            y_pos = 350 + i * 60
            
            # Highlight selected option
            if i == self.menu_selection:
                highlight_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, y_pos - 15, 400, 40)
                pygame.draw.rect(self.screen, BLUE, highlight_rect, 2)
                color = YELLOW
                
                # Add glow to selected option
                glow_text = self.font_large.render(option, True, BLUE)
                glow_rect = glow_text.get_rect(center=(WINDOW_WIDTH // 2 + 2, y_pos + 2))
                self.screen.blit(glow_text, glow_rect)
            else:
                color = WHITE
            
            option_text = self.font_large.render(option, True, color)
            option_rect = option_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos))
            self.screen.blit(option_text, option_rect)
        
        # Instructions
        instructions = [
            "Use ↑↓ to navigate, ENTER to select",
            "Test your typing speed and accuracy!"
        ]
        
        for i, instruction in enumerate(instructions):
            inst_text = self.font_small.render(instruction, True, GRAY)
            inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 650 + i * 30))
            self.screen.blit(inst_text, inst_rect)
    
    def draw_difficulty_select(self):
        """Draw difficulty selection screen"""
        self.draw_background()
        
        # Title
        title = self.font_large.render("Select Difficulty Level", True, WHITE)
        title_rect = title.get_rect(center=(WINDOW_WIDTH // 2, 150))
        self.screen.blit(title, title_rect)
        
        # Difficulty options
        difficulties = list(Difficulty)
        
        for i, difficulty in enumerate(difficulties):
            y_pos = 250 + i * 100
            
            # Highlight selected difficulty
            if difficulty == self.current_difficulty:
                highlight_rect = pygame.Rect(WINDOW_WIDTH // 2 - 300, y_pos - 30, 600, 80)
                pygame.draw.rect(self.screen, BLUE, highlight_rect, 3)
                
                # Add animated border
                border_glow = int(50 + 30 * math.sin(time.time() * 4))
                border_color = (*CYAN, border_glow)
                border_surface = pygame.Surface((highlight_rect.width + 10, highlight_rect.height + 10), pygame.SRCALPHA)
                pygame.draw.rect(border_surface, border_color, border_surface.get_rect(), 2)
                self.screen.blit(border_surface, (highlight_rect.x - 5, highlight_rect.y - 5))
            
            # Difficulty info
            name_text = self.font_medium.render(difficulty.value[0], True, WHITE)
            name_rect = name_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos))
            self.screen.blit(name_text, name_rect)
            
            desc_text = self.font_small.render(difficulty.value[1], True, GRAY)
            desc_rect = desc_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos + 30))
            self.screen.blit(desc_text, desc_rect)
            
            time_text = self.font_small.render(f"~{difficulty.value[2]} seconds", True, YELLOW)
            time_rect = time_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos + 50))
            self.screen.blit(time_text, time_rect)
        
        # Instructions
        instructions = [
            "↑↓ to select difficulty, ENTER to start, ESC to go back"
        ]
        
        inst_text = self.font_small.render(instructions[0], True, WHITE)
        inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 700))
        self.screen.blit(inst_text, inst_rect)
    
    def draw_typing_test(self):
        """Draw typing test screen"""
        self.draw_background()
        
        # Real-time stats
        current_wpm = self.calculate_real_time_wpm()
        current_accuracy = (self.correct_characters / self.total_characters * 100) if self.total_characters > 0 else 100.0
        
        # Stats panel
        stats_y = 50
        wpm_text = self.font_medium.render(f"WPM: {current_wpm}", True, YELLOW)
        self.screen.blit(wpm_text, (50, stats_y))
        
        accuracy_text = self.font_medium.render(f"Accuracy: {current_accuracy:.1f}%", True, GREEN if current_accuracy >= 95 else ORANGE if current_accuracy >= 90 else RED)
        self.screen.blit(accuracy_text, (250, stats_y))
        
        errors_text = self.font_medium.render(f"Errors: {self.errors}", True, RED)
        self.screen.blit(errors_text, (500, stats_y))
        
        # Progress bar
        progress = self.current_position / len(self.test_text) if self.test_text else 0
        progress_rect = pygame.Rect(50, stats_y + 40, WINDOW_WIDTH - 100, 10)
        pygame.draw.rect(self.screen, DARK_GRAY, progress_rect)
        
        if progress > 0:
            progress_fill = pygame.Rect(50, stats_y + 40, int((WINDOW_WIDTH - 100) * progress), 10)
            color = GREEN if current_accuracy >= 95 else ORANGE if current_accuracy >= 90 else RED
            pygame.draw.rect(self.screen, color, progress_fill)
        
        # Text display area
        text_area_y = 150
        text_area_height = 200
        
        # Background for text area
        text_bg = pygame.Rect(50, text_area_y, WINDOW_WIDTH - 100, text_area_height)
        pygame.draw.rect(self.screen, (40, 40, 50), text_bg)
        pygame.draw.rect(self.screen, WHITE, text_bg, 2)
        
        # Render text with highlighting
        self.draw_text_with_highlighting(60, text_area_y + 20, WINDOW_WIDTH - 120)
        
        # Instructions
        if not self.is_started:
            instruction = self.font_small.render("Start typing to begin the test...", True, WHITE)
            instruction_rect = instruction.get_rect(center=(WINDOW_WIDTH // 2, 400))
            self.screen.blit(instruction, instruction_rect)
        
        # Current typing intensity visualization
        if self.typing_intensity > 0:
            intensity_radius = int(20 + self.typing_intensity * 30)
            intensity_alpha = int(100 * self.typing_intensity)
            
            intensity_surface = pygame.Surface((intensity_radius * 4, intensity_radius * 4), pygame.SRCALPHA)
            intensity_color = (*CYAN, intensity_alpha)
            pygame.draw.circle(intensity_surface, intensity_color, (intensity_radius * 2, intensity_radius * 2), intensity_radius)
            self.screen.blit(intensity_surface, (WINDOW_WIDTH // 2 - intensity_radius * 2, 450))
    
    def draw_text_with_highlighting(self, x, y, max_width):
        """Draw text with color coding for correct/incorrect characters"""
        if not self.test_text:
            return
        
        line_height = 35
        current_line = 0
        char_x = x
        words = self.test_text.split(' ')
        char_index = 0
        
        for word_index, word in enumerate(words):
            # Check if word fits on current line
            word_surface = self.font_medium.render(word + ' ', True, WHITE)
            if char_x + word_surface.get_width() > x + max_width and char_x > x:
                # Move to next line
                current_line += 1
                char_x = x
                y += line_height
            
            # Draw each character in the word
            for char in word:
                color = WHITE  # Default color
                bg_color = None
                
                if char_index < self.current_position:
                    # Already typed
                    if char_index < len(self.user_input):
                        if char_index < len(self.user_input) and self.user_input[char_index] == char:
                            color = GREEN  # Correct
                        else:
                            color = RED  # Error
                            bg_color = (100, 50, 50)  # Red background for errors
                elif char_index == self.current_position:
                    # Current character
                    bg_color = (100, 100, 100)  # Gray background for current
                    color = YELLOW
                else:
                    # Not yet typed
                    color = LIGHT_GRAY
                
                # Draw background if needed
                if bg_color:
                    char_surface = self.font_medium.render(char, True, color)
                    bg_rect = pygame.Rect(char_x - 2, y - 2, char_surface.get_width() + 4, char_surface.get_height() + 4)
                    pygame.draw.rect(self.screen, bg_color, bg_rect)
                
                # Draw character
                char_surface = self.font_medium.render(char, True, color)
                self.screen.blit(char_surface, (char_x, y))
                char_x += char_surface.get_width()
                char_index += 1
            
            # Add space after word
            if word_index < len(words) - 1:
                space_surface = self.font_medium.render(' ', True, WHITE)
                
                # Handle space highlighting
                if char_index < self.current_position:
                    if char_index < len(self.user_input) and self.user_input[char_index] == ' ':
                        color = GREEN
                    else:
                        color = RED
                        bg_rect = pygame.Rect(char_x - 2, y - 2, space_surface.get_width() + 4, space_surface.get_height() + 4)
                        pygame.draw.rect(self.screen, (100, 50, 50), bg_rect)
                elif char_index == self.current_position:
                    bg_rect = pygame.Rect(char_x - 2, y - 2, space_surface.get_width() + 4, space_surface.get_height() + 4)
                    pygame.draw.rect(self.screen, (100, 100, 100), bg_rect)
                    color = YELLOW
                else:
                    color = LIGHT_GRAY
                
                space_surface = self.font_medium.render('_', True, color)  # Show space as underscore
                self.screen.blit(space_surface, (char_x, y))
                char_x += self.font_medium.render(' ', True, WHITE).get_width()
                char_index += 1
    
    def draw_results(self):
        """Draw results screen"""
        self.draw_background()
        
        # Results title
        title = self.font_large.render("📊 Test Results", True, WHITE)
        title_rect = title.get_rect(center=(WINDOW_WIDTH // 2, 100))
        self.screen.blit(title, title_rect)
        
        # Results panel
        panel_rect = pygame.Rect(WINDOW_WIDTH // 2 - 300, 200, 600, 300)
        pygame.draw.rect(self.screen, (40, 40, 50), panel_rect)
        pygame.draw.rect(self.screen, WHITE, panel_rect, 3)
        
        # Results data
        results = [
            ("Words Per Minute", f"{self.wpm}", YELLOW),
            ("Accuracy", f"{self.accuracy:.1f}%", GREEN if self.accuracy >= 95 else ORANGE if self.accuracy >= 90 else RED),
            ("Errors", f"{self.errors}", RED),
            ("Characters Typed", f"{self.total_characters}", WHITE),
            ("Test Duration", f"{self.end_time - self.start_time:.1f}s", WHITE)
        ]
        
        y_offset = 230
        for label, value, color in results:
            label_text = self.font_medium.render(f"{label}:", True, WHITE)
            value_text = self.font_medium.render(value, True, color)
            
            self.screen.blit(label_text, (WINDOW_WIDTH // 2 - 280, y_offset))
            self.screen.blit(value_text, (WINDOW_WIDTH // 2 + 50, y_offset))
            y_offset += 40
        
        # Performance rating
        rating = self.get_performance_rating()
        rating_text = self.font_large.render(rating, True, YELLOW)
        rating_rect = rating_text.get_rect(center=(WINDOW_WIDTH // 2, 520))
        self.screen.blit(rating_text, rating_rect)
        
        # Instructions
        instructions = [
            "ENTER - Back to Menu | SPACE - Try Again"
        ]
        
        inst_text = self.font_small.render(instructions[0], True, WHITE)
        inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 600))
        self.screen.blit(inst_text, inst_rect)
    
    def get_performance_rating(self):
        """Get performance rating based on WPM and accuracy"""
        if self.wpm >= 80 and self.accuracy >= 98:
            return "🏆 MASTER TYPIST!"
        elif self.wpm >= 60 and self.accuracy >= 95:
            return "⭐ EXCELLENT!"
        elif self.wpm >= 40 and self.accuracy >= 90:
            return "👍 GOOD JOB!"
        elif self.wpm >= 25 and self.accuracy >= 85:
            return "📈 KEEP PRACTICING!"
        else:
            return "🎯 ROOM FOR IMPROVEMENT"
    
    def draw_statistics(self):
        """Draw statistics screen"""
        self.draw_background()
        
        # Statistics title
        title = self.font_large.render("📈 Your Statistics", True, WHITE)
        title_rect = title.get_rect(center=(WINDOW_WIDTH // 2, 100))
        self.screen.blit(title, title_rect)
        
        # Statistics panel
        panel_rect = pygame.Rect(WINDOW_WIDTH // 2 - 400, 150, 800, 400)
        pygame.draw.rect(self.screen, (40, 40, 50), panel_rect)
        pygame.draw.rect(self.screen, WHITE, panel_rect, 3)
        
        # Overall statistics
        overall_stats = [
            ("Games Played", f"{self.statistics['games_played']}"),
            ("Best WPM", f"{self.statistics['best_wpm']:.0f}"),
            ("Best Accuracy", f"{self.statistics['best_accuracy']:.1f}%"),
            ("Average WPM", f"{self.statistics['average_wpm']:.1f}"),
            ("Total Words Typed", f"{self.statistics['total_words_typed']}")
        ]
        
        y_offset = 180
        
        # Overall stats section
        section_title = self.font_medium.render("Overall Performance", True, CYAN)
        self.screen.blit(section_title, (WINDOW_WIDTH // 2 - 380, y_offset))
        y_offset += 40
        
        for label, value in overall_stats:
            label_text = self.font_small.render(f"{label}:", True, WHITE)
            value_text = self.font_small.render(value, True, YELLOW)
            
            self.screen.blit(label_text, (WINDOW_WIDTH // 2 - 380, y_offset))
            self.screen.blit(value_text, (WINDOW_WIDTH // 2 - 150, y_offset))
            y_offset += 30
        
        # Difficulty-specific stats
        y_offset = 180
        difficulty_title = self.font_medium.render("Difficulty Breakdown", True, CYAN)
        self.screen.blit(difficulty_title, (WINDOW_WIDTH // 2 + 50, y_offset))
        y_offset += 40
        
        for difficulty in Difficulty:
            diff_stats = self.statistics['difficulty_stats'][difficulty.name]
            diff_text = f"{difficulty.value[0]}: {diff_stats['games']} games, {diff_stats['best_wpm']:.0f} WPM"
            
            stats_text = self.font_small.render(diff_text, True, WHITE)
            self.screen.blit(stats_text, (WINDOW_WIDTH // 2 + 50, y_offset))
            y_offset += 30
        
        # Instructions
        inst_text = self.font_small.render("Press ESC to return to menu", True, WHITE)
        inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 700))
        self.screen.blit(inst_text, inst_rect)
    
    def draw(self):
        """Main draw function"""
        if self.game_state == GameState.MENU:
            self.draw_menu()
        elif self.game_state == GameState.DIFFICULTY_SELECT:
            self.draw_difficulty_select()
        elif self.game_state == GameState.TYPING_TEST:
            self.draw_typing_test()
        elif self.game_state == GameState.RESULTS:
            self.draw_results()
        elif self.game_state == GameState.STATISTICS:
            self.draw_statistics()
        
        # Draw particles on top
        for particle in self.particles:
            particle.draw(self.screen)
        
        pygame.display.flip()
    
    def run(self):
        """Main game loop"""
        running = True
        
        while running:
            running = self.handle_events()
            self.update()
            self.draw()
            self.clock.tick(FPS)
        
        pygame.quit()
        sys.exit()

def main():
    print("⌨️ Typing Speed Test - Ultimate Edition")
    print("=" * 45)
    print("Features:")
    print("• Accurate WPM (Words Per Minute) calculation")
    print("• Real-time accuracy percentage tracking")
    print("• Multiple difficulty levels:")
    print("  - Beginner: Simple words and phrases")
    print("  - Intermediate: Mixed sentences")  
    print("  - Advanced: Complex paragraphs")
    print("  - Expert: Technical content")
    print("• Visual feedback with color-coded text")
    print("• Animated skeletal creature (inspired by your image)")
    print("• Particle effects and smooth animations")
    print("• Comprehensive statistics tracking")
    print("• Professional performance ratings")
    print("• Sound effects for typing feedback")
    print("• Progress visualization and real-time stats")
    print("\nHow it works:")
    print("• WPM = (Total words typed / Time in minutes)")
    print("• Accuracy = (Correct characters / Total characters) × 100%")
    print("• Real-time feedback shows your current performance")
    print("• Statistics are saved automatically")
    print("\nControls:")
    print("• Arrow keys - Navigate menus")
    print("• ENTER - Select options")
    print("• Type normally during tests")
    print("• BACKSPACE - Correct mistakes")
    print("• ESC - Back/Exit")
    print("\nStarting Typing Speed Test...")
    
    try:
        game = TypingSpeedTest()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()