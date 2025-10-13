import pygame
import sys
import random
import math
import time
from enum import Enum

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 1000
WINDOW_HEIGHT = 600
PADDLE_WIDTH = 15
PADDLE_HEIGHT = 80
BALL_SIZE = 12
FPS = 60

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GRAY = (128, 128, 128)
LIGHT_GRAY = (200, 200, 200)
DARK_GRAY = (64, 64, 64)
BLUE = (100, 150, 255)
RED = (255, 100, 100)
GREEN = (100, 255, 100)
YELLOW = (255, 255, 100)
PURPLE = (200, 100, 255)
ORANGE = (255, 150, 50)
CYAN = (100, 255, 255)

class GameMode(Enum):
    MENU = "menu"
    SINGLE_PLAYER = "single_player"
    TWO_PLAYER = "two_player"
    SURVIVAL = "survival"
    GAME_OVER = "game_over"
    PAUSED = "paused"

class PowerUpType(Enum):
    SPEED_BOOST = "speed_boost"
    BIG_PADDLE = "big_paddle"
    SMALL_PADDLE = "small_paddle"
    MULTI_BALL = "multi_ball"
    SLOW_MOTION = "slow_motion"

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
        self.spin_speed = random.uniform(-0.2, 0.2)
        
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
            
            # Create glowing particle effect
            surface = pygame.Surface((size * 4, size * 4), pygame.SRCALPHA)
            
            # Outer glow
            for i in range(3, 0, -1):
                glow_alpha = alpha // (i + 1)
                glow_color = (*self.color, glow_alpha)
                pygame.draw.circle(surface, glow_color, (size * 2, size * 2), size + i)
            
            # Core particle
            core_color = (*self.color, alpha)
            pygame.draw.circle(surface, core_color, (size * 2, size * 2), size)
            
            screen.blit(surface, (self.x - size * 2, self.y - size * 2))

class TrailSegment:
    def __init__(self, x, y, life=30):
        self.x = x
        self.y = y
        self.life = life
        self.max_life = life

class Ball:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.vx = random.choice([-5, 5])
        self.vy = random.uniform(-3, 3)
        self.size = BALL_SIZE
        self.trail = []
        self.glow_radius = 0
        self.color = WHITE
        self.speed_multiplier = 1.0
        
    def update(self):
        # Update position
        self.x += self.vx * self.speed_multiplier
        self.y += self.vy * self.speed_multiplier
        
        # Update trail
        self.trail.append(TrailSegment(self.x, self.y))
        if len(self.trail) > 15:
            self.trail.pop(0)
        
        # Update trail life
        for segment in self.trail:
            segment.life -= 1
        self.trail = [s for s in self.trail if s.life > 0]
        
        # Update glow effect
        self.glow_radius = (self.glow_radius + 0.2) % (math.pi * 2)
        
        # Boundary collision (top and bottom)
        if self.y <= self.size or self.y >= WINDOW_HEIGHT - self.size:
            self.vy = -self.vy
            return "wall_hit"
        
        # Check if ball goes off screen
        if self.x < -self.size:
            return "right_scores"
        elif self.x > WINDOW_WIDTH + self.size:
            return "left_scores"
            
        return None
    
    def draw(self, screen):
        # Draw trail
        for i, segment in enumerate(self.trail):
            alpha = int(100 * (segment.life / segment.max_life))
            size = max(1, int(self.size * (segment.life / segment.max_life)))
            
            trail_surface = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            trail_color = (*self.color, alpha)
            pygame.draw.circle(trail_surface, trail_color, (size, size), size)
            screen.blit(trail_surface, (segment.x - size, segment.y - size))
        
        # Draw ball glow
        glow_size = int(self.size + 5 + 3 * math.sin(self.glow_radius))
        glow_surface = pygame.Surface((glow_size * 3, glow_size * 3), pygame.SRCALPHA)
        
        # Multi-layered glow
        for i in range(5, 0, -1):
            glow_alpha = 30 // i
            glow_color = (*self.color, glow_alpha)
            pygame.draw.circle(glow_surface, glow_color, 
                             (glow_size * 1.5, glow_size * 1.5), glow_size + i)
        
        screen.blit(glow_surface, (self.x - glow_size * 1.5, self.y - glow_size * 1.5))
        
        # Draw main ball
        pygame.draw.circle(screen, self.color, (int(self.x), int(self.y)), self.size)
        pygame.draw.circle(screen, WHITE, 
                         (int(self.x - 3), int(self.y - 3)), self.size // 3)

class Paddle:
    def __init__(self, x, y, is_ai=False):
        self.x = x
        self.y = y
        self.width = PADDLE_WIDTH
        self.height = PADDLE_HEIGHT
        self.speed = 6
        self.is_ai = is_ai
        self.target_y = y
        self.ai_difficulty = 0.8
        self.glow_intensity = 0
        self.color = WHITE
        self.power_up_timer = 0
        self.power_up_type = None
        
    def update(self, ball=None):
        if self.is_ai and ball:
            # AI movement with some error for realism
            error = random.uniform(-20, 20) * (1 - self.ai_difficulty)
            self.target_y = ball.y + error
            
            # Smooth AI movement
            diff = self.target_y - self.y
            if abs(diff) > 2:
                self.y += math.copysign(min(self.speed, abs(diff)), diff)
        
        # Keep paddle on screen
        self.y = max(self.height // 2, min(WINDOW_HEIGHT - self.height // 2, self.y))
        
        # Update power-up timer
        if self.power_up_timer > 0:
            self.power_up_timer -= 1
            if self.power_up_timer == 0:
                self.remove_power_up()
        
        # Update glow
        self.glow_intensity = (self.glow_intensity + 0.1) % (math.pi * 2)
    
    def move_up(self):
        if not self.is_ai:
            self.y -= self.speed
    
    def move_down(self):
        if not self.is_ai:
            self.y += self.speed
    
    def apply_power_up(self, power_up_type):
        self.power_up_type = power_up_type
        self.power_up_timer = 300  # 5 seconds at 60 FPS
        
        if power_up_type == PowerUpType.BIG_PADDLE:
            self.height = PADDLE_HEIGHT * 1.5
            self.color = GREEN
        elif power_up_type == PowerUpType.SMALL_PADDLE:
            self.height = PADDLE_HEIGHT * 0.7
            self.color = RED
        elif power_up_type == PowerUpType.SPEED_BOOST:
            self.speed = 10
            self.color = YELLOW
    
    def remove_power_up(self):
        self.power_up_type = None
        self.height = PADDLE_HEIGHT
        self.speed = 6
        self.color = WHITE
    
    def get_rect(self):
        return pygame.Rect(self.x - self.width // 2, self.y - self.height // 2, 
                          self.width, self.height)
    
    def draw(self, screen):
        rect = self.get_rect()
        
        # Draw paddle glow
        if self.power_up_timer > 0:
            glow_size = int(5 + 3 * math.sin(self.glow_intensity * 3))
            glow_rect = pygame.Rect(rect.x - glow_size, rect.y - glow_size,
                                   rect.width + glow_size * 2, rect.height + glow_size * 2)
            
            glow_surface = pygame.Surface((glow_rect.width + 20, glow_rect.height + 20), pygame.SRCALPHA)
            for i in range(3, 0, -1):
                glow_alpha = 50 // i
                glow_color = (*self.color, glow_alpha)
                pygame.draw.rect(glow_surface, glow_color, 
                               (10 - i, 10 - i, glow_rect.width + i * 2, glow_rect.height + i * 2))
            
            screen.blit(glow_surface, (glow_rect.x - 10, glow_rect.y - 10))
        
        # Draw main paddle
        pygame.draw.rect(screen, self.color, rect)
        pygame.draw.rect(screen, WHITE, rect, 2)
        
        # Draw 3D effect
        pygame.draw.line(screen, WHITE, (rect.left, rect.top), (rect.right, rect.top), 2)
        pygame.draw.line(screen, DARK_GRAY, (rect.left, rect.bottom), (rect.right, rect.bottom), 2)

class PowerUp:
    def __init__(self, x, y, power_type):
        self.x = x
        self.y = y
        self.type = power_type
        self.size = 20
        self.rotation = 0
        self.pulse = 0
        self.life = 600  # 10 seconds
        
        self.colors = {
            PowerUpType.SPEED_BOOST: YELLOW,
            PowerUpType.BIG_PADDLE: GREEN,
            PowerUpType.SMALL_PADDLE: RED,
            PowerUpType.MULTI_BALL: PURPLE,
            PowerUpType.SLOW_MOTION: CYAN
        }
        
        self.symbols = {
            PowerUpType.SPEED_BOOST: "⚡",
            PowerUpType.BIG_PADDLE: "⬆",
            PowerUpType.SMALL_PADDLE: "⬇",
            PowerUpType.MULTI_BALL: "●",
            PowerUpType.SLOW_MOTION: "🕐"
        }
    
    def update(self):
        self.rotation += 5
        self.pulse += 0.2
        self.life -= 1
        return self.life > 0
    
    def get_rect(self):
        return pygame.Rect(self.x - self.size, self.y - self.size, 
                          self.size * 2, self.size * 2)
    
    def draw(self, screen):
        color = self.colors[self.type]
        size = int(self.size + 5 * math.sin(self.pulse))
        
        # Draw glow
        glow_surface = pygame.Surface((size * 3, size * 3), pygame.SRCALPHA)
        for i in range(3, 0, -1):
            glow_alpha = 50 // i
            glow_color = (*color, glow_alpha)
            pygame.draw.circle(glow_surface, glow_color, (size * 1.5, size * 1.5), size + i * 3)
        
        screen.blit(glow_surface, (self.x - size * 1.5, self.y - size * 1.5))
        
        # Draw power-up icon
        pygame.draw.circle(screen, color, (int(self.x), int(self.y)), size)
        pygame.draw.circle(screen, WHITE, (int(self.x), int(self.y)), size, 2)
        
        # Draw symbol (simplified representation)
        if self.type == PowerUpType.SPEED_BOOST:
            # Lightning bolt shape
            points = [(self.x - 5, self.y - 8), (self.x + 2, self.y - 2), 
                     (self.x - 2, self.y), (self.x + 5, self.y + 8)]
            pygame.draw.lines(screen, WHITE, False, points, 3)
        elif self.type == PowerUpType.BIG_PADDLE:
            # Up arrow
            points = [(self.x, self.y - 8), (self.x - 6, self.y + 2), (self.x + 6, self.y + 2)]
            pygame.draw.polygon(screen, WHITE, points)
        elif self.type == PowerUpType.SMALL_PADDLE:
            # Down arrow
            points = [(self.x, self.y + 8), (self.x - 6, self.y - 2), (self.x + 6, self.y - 2)]
            pygame.draw.polygon(screen, WHITE, points)

class SkeletalCreature:
    """Inspired by the skeletal creature in the image"""
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.nodes = []
        self.segments = 20
        self.segment_length = 15
        self.target_x = x
        self.target_y = y
        
        # Initialize spine nodes
        for i in range(self.segments):
            node_x = x
            node_y = y + i * self.segment_length
            self.nodes.append({'x': node_x, 'y': node_y, 'angle': 0})
    
    def moveTo(self, x, y):
        """Move creature towards target position with smooth following"""
        self.target_x = x
        self.target_y = y
        
        if not self.nodes:
            return
        
        # Calculate distance to target
        dist = math.sqrt((x - self.nodes[0]['x'])**2 + (y - self.nodes[0]['y'])**2)
        
        # Move head towards target
        if dist > 5:
            speed = min(3, dist * 0.1)
            angle = math.atan2(y - self.nodes[0]['y'], x - self.nodes[0]['x'])
            self.nodes[0]['x'] += speed * math.cos(angle)
            self.nodes[0]['y'] += speed * math.sin(angle)
        
        # Update following segments
        for i in range(1, len(self.nodes)):
            prev_node = self.nodes[i-1]
            current_node = self.nodes[i]
            
            dx = prev_node['x'] - current_node['x']
            dy = prev_node['y'] - current_node['y']
            distance = math.sqrt(dx**2 + dy**2)
            
            if distance > self.segment_length:
                angle = math.atan2(dy, dx)
                current_node['x'] = prev_node['x'] - self.segment_length * math.cos(angle)
                current_node['y'] = prev_node['y'] - self.segment_length * math.sin(angle)
                current_node['angle'] = angle
    
    def update(self, target_x, target_y):
        self.moveTo(target_x, target_y)
    
    def draw(self, screen):
        if not self.nodes:
            return
        
        # Draw spine
        for i in range(len(self.nodes) - 1):
            start = (int(self.nodes[i]['x']), int(self.nodes[i]['y']))
            end = (int(self.nodes[i+1]['x']), int(self.nodes[i+1]['y']))
            
            # Fade effect along the spine
            alpha = int(255 * (1 - i / len(self.nodes)))
            color = (*WHITE, alpha)
            
            # Draw spine segment with glow
            pygame.draw.line(screen, WHITE, start, end, 3)
        
        # Draw ribs
        for i, node in enumerate(self.nodes):
            if i % 3 == 0 and i > 0:  # Draw ribs every 3rd segment
                rib_length = 20 - i * 0.5
                angle = node['angle'] + math.pi / 2
                
                # Left rib
                rib_end_x = node['x'] + rib_length * math.cos(angle)
                rib_end_y = node['y'] + rib_length * math.sin(angle)
                pygame.draw.line(screen, WHITE, 
                               (int(node['x']), int(node['y'])), 
                               (int(rib_end_x), int(rib_end_y)), 2)
                
                # Right rib
                rib_end_x = node['x'] - rib_length * math.cos(angle)
                rib_end_y = node['y'] - rib_length * math.sin(angle)
                pygame.draw.line(screen, WHITE, 
                               (int(node['x']), int(node['y'])), 
                               (int(rib_end_x), int(rib_end_y)), 2)
        
        # Draw nodes
        for i, node in enumerate(self.nodes):
            size = max(2, 8 - i // 3)
            alpha = int(255 * (1 - i / len(self.nodes) * 0.5))
            
            node_surface = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            color = (*WHITE, alpha)
            pygame.draw.circle(node_surface, color, (size, size), size)
            screen.blit(node_surface, (node['x'] - size, node['y'] - size))

class EnhancedPongGame:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🏓 Enhanced Pong - Ultimate Edition")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.font_large = pygame.font.Font(None, 72)
        self.font_medium = pygame.font.Font(None, 48)
        self.font_small = pygame.font.Font(None, 36)
        self.font_tiny = pygame.font.Font(None, 24)
        
        # Game objects
        self.left_paddle = Paddle(50, WINDOW_HEIGHT // 2)
        self.right_paddle = Paddle(WINDOW_WIDTH - 50, WINDOW_HEIGHT // 2, is_ai=True)
        self.balls = [Ball(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2)]
        self.power_ups = []
        self.particles = []
        
        # Skeletal creature (decorative)
        self.creature = SkeletalCreature(WINDOW_WIDTH // 2, WINDOW_HEIGHT - 100)
        
        # Game state
        self.game_mode = GameMode.MENU
        self.left_score = 0
        self.right_score = 0
        self.max_score = 5
        self.menu_selection = 0
        
        # Effects
        self.screen_shake = 0
        self.slow_motion_timer = 0
        self.background_particles = []
        
        # Initialize background particles
        for _ in range(50):
            x = random.randint(0, WINDOW_WIDTH)
            y = random.randint(0, WINDOW_HEIGHT)
            velocity = (random.uniform(-0.5, 0.5), random.uniform(-0.5, 0.5))
            color = random.choice([DARK_GRAY, GRAY])
            self.background_particles.append(Particle(x, y, color, velocity, life=1000))
        
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
        """Play sound effects"""
        if not self.sound_enabled:
            return
        
        try:
            if sound_type == "paddle_hit":
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 2000 * math.sin(2 * math.pi * 400 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "wall_hit":
                duration = 0.15
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 1500 * math.sin(2 * math.pi * 300 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "score":
                duration = 0.5
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 440 + (i * 220 / frames)
                    wave = 1000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "power_up":
                duration = 0.3
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 600 + 400 * math.sin(i * 0.01)
                    wave = 800 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def create_explosion(self, x, y, color, count=15):
        """Create particle explosion effect"""
        for _ in range(count):
            velocity = (random.uniform(-8, 8), random.uniform(-8, 8))
            self.particles.append(Particle(x, y, color, velocity, life=60))
    
    def spawn_power_up(self):
        """Randomly spawn power-ups"""
        if len(self.power_ups) < 2 and random.randint(1, 300) == 1:
            x = random.randint(200, WINDOW_WIDTH - 200)
            y = random.randint(100, WINDOW_HEIGHT - 100)
            power_type = random.choice(list(PowerUpType))
            self.power_ups.append(PowerUp(x, y, power_type))
    
    def check_ball_paddle_collision(self, ball, paddle):
        """Check collision between ball and paddle"""
        ball_rect = pygame.Rect(ball.x - ball.size, ball.y - ball.size, 
                               ball.size * 2, ball.size * 2)
        paddle_rect = paddle.get_rect()
        
        if ball_rect.colliderect(paddle_rect):
            # Calculate hit position on paddle (-1 to 1)
            hit_pos = (ball.y - paddle.y) / (paddle.height / 2)
            hit_pos = max(-1, min(1, hit_pos))
            
            # Reverse horizontal direction
            ball.vx = -ball.vx
            
            # Adjust vertical velocity based on hit position
            ball.vy += hit_pos * 3
            
            # Limit velocity
            ball.vy = max(-8, min(8, ball.vy))
            
            # Increase speed slightly
            speed = math.sqrt(ball.vx**2 + ball.vy**2)
            if speed < 12:
                ball.vx *= 1.05
                ball.vy *= 1.05
            
            # Create hit effect
            self.create_explosion(ball.x, ball.y, paddle.color, 8)
            self.screen_shake = 5
            self.play_sound("paddle_hit")
            
            return True
        return False
    
    def check_power_up_collision(self, paddle):
        """Check if paddle collected a power-up"""
        paddle_rect = paddle.get_rect()
        
        for power_up in self.power_ups[:]:
            if paddle_rect.colliderect(power_up.get_rect()):
                self.power_ups.remove(power_up)
                paddle.apply_power_up(power_up.type)
                self.create_explosion(power_up.x, power_up.y, power_up.colors[power_up.type], 12)
                self.play_sound("power_up")
                
                # Special power-up effects
                if power_up.type == PowerUpType.MULTI_BALL and len(self.balls) < 3:
                    new_ball = Ball(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2)
                    new_ball.color = PURPLE
                    self.balls.append(new_ball)
                elif power_up.type == PowerUpType.SLOW_MOTION:
                    self.slow_motion_timer = 180  # 3 seconds
                
                return True
        return False
    
    def update_game(self):
        """Update game logic"""
        if self.game_mode not in [GameMode.SINGLE_PLAYER, GameMode.TWO_PLAYER, GameMode.SURVIVAL]:
            return
        
        # Update time effects
        time_scale = 0.3 if self.slow_motion_timer > 0 else 1.0
        if self.slow_motion_timer > 0:
            self.slow_motion_timer -= 1
        
        # Update paddles
        self.left_paddle.update(self.balls[0] if self.balls else None)
        self.right_paddle.update(self.balls[0] if self.balls else None)
        
        # Update balls
        for ball in self.balls[:]:
            ball.speed_multiplier = time_scale
            result = ball.update()
            
            if result == "wall_hit":
                self.play_sound("wall_hit")
                self.create_explosion(ball.x, ball.y, WHITE, 5)
            elif result == "left_scores":
                self.right_score += 1
                self.play_sound("score")
                if len(self.balls) > 1:
                    self.balls.remove(ball)
                else:
                    self.reset_ball(ball)
                self.check_game_over()
            elif result == "right_scores":
                self.left_score += 1
                self.play_sound("score")
                if len(self.balls) > 1:
                    self.balls.remove(ball)
                else:
                    self.reset_ball(ball)
                self.check_game_over()
        
        # Check collisions
        for ball in self.balls:
            self.check_ball_paddle_collision(ball, self.left_paddle)
            self.check_ball_paddle_collision(ball, self.right_paddle)
        
        # Check power-up collisions
        self.check_power_up_collision(self.left_paddle)
        self.check_power_up_collision(self.right_paddle)
        
        # Update power-ups
        self.power_ups = [p for p in self.power_ups if p.update()]
        self.spawn_power_up()
        
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
                particle.life = 1000
        
        # Update creature
        if self.balls:
            self.creature.update(self.balls[0].x, self.balls[0].y - 100)
        
        # Update screen shake
        if self.screen_shake > 0:
            self.screen_shake -= 1
    
    def reset_ball(self, ball):
        """Reset ball to center"""
        ball.x = WINDOW_WIDTH // 2
        ball.y = WINDOW_HEIGHT // 2
        ball.vx = random.choice([-5, 5])
        ball.vy = random.uniform(-3, 3)
        ball.speed_multiplier = 1.0
        ball.trail.clear()
    
    def check_game_over(self):
        """Check if game is over"""
        if self.left_score >= self.max_score or self.right_score >= self.max_score:
            self.game_mode = GameMode.GAME_OVER
    
    def handle_events(self):
        """Handle game events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    if self.game_mode in [GameMode.SINGLE_PLAYER, GameMode.TWO_PLAYER, GameMode.SURVIVAL]:
                        self.game_mode = GameMode.PAUSED
                    elif self.game_mode == GameMode.PAUSED:
                        self.game_mode = GameMode.MENU
                    elif self.game_mode == GameMode.GAME_OVER:
                        self.game_mode = GameMode.MENU
                    elif self.game_mode == GameMode.MENU:
                        return False
                
                elif event.key == pygame.K_SPACE:
                    if self.game_mode == GameMode.PAUSED:
                        # Resume previous game mode
                        if hasattr(self, 'previous_mode'):
                            self.game_mode = self.previous_mode
                        else:
                            self.game_mode = GameMode.SINGLE_PLAYER
                    elif self.game_mode == GameMode.GAME_OVER:
                        self.reset_game()
                        self.game_mode = GameMode.SINGLE_PLAYER
                
                elif self.game_mode == GameMode.MENU:
                    if event.key == pygame.K_UP:
                        self.menu_selection = (self.menu_selection - 1) % 4
                    elif event.key == pygame.K_DOWN:
                        self.menu_selection = (self.menu_selection + 1) % 4
                    elif event.key == pygame.K_RETURN:
                        if self.menu_selection == 0:  # Single Player
                            self.reset_game()
                            self.right_paddle.is_ai = True
                            self.game_mode = GameMode.SINGLE_PLAYER
                        elif self.menu_selection == 1:  # Two Player
                            self.reset_game()
                            self.right_paddle.is_ai = False
                            self.game_mode = GameMode.TWO_PLAYER
                        elif self.menu_selection == 2:  # Survival
                            self.reset_game()
                            self.max_score = 10
                            self.right_paddle.ai_difficulty = 0.9
                            self.game_mode = GameMode.SURVIVAL
                        elif self.menu_selection == 3:  # Exit
                            return False
        
        # Handle continuous key presses
        keys = pygame.key.get_pressed()
        
        if self.game_mode in [GameMode.SINGLE_PLAYER, GameMode.TWO_PLAYER, GameMode.SURVIVAL]:
            # Left paddle controls
            if keys[pygame.K_w]:
                self.left_paddle.move_up()
            if keys[pygame.K_s]:
                self.left_paddle.move_down()
            
            # Right paddle controls (only in two player mode)
            if self.game_mode == GameMode.TWO_PLAYER:
                if keys[pygame.K_UP]:
                    self.right_paddle.move_up()
                if keys[pygame.K_DOWN]:
                    self.right_paddle.move_down()
        
        return True
    
    def reset_game(self):
        """Reset game to initial state"""
        self.left_score = 0
        self.right_score = 0
        self.balls = [Ball(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2)]
        self.power_ups = []
        self.particles = []
        self.left_paddle.remove_power_up()
        self.right_paddle.remove_power_up()
        self.slow_motion_timer = 0
        self.max_score = 5
    
    def draw_background(self):
        """Draw animated background"""
        self.screen.fill(BLACK)
        
        # Draw background particles
        for particle in self.background_particles:
            particle.draw(self.screen)
        
        # Draw center line
        for y in range(0, WINDOW_HEIGHT, 20):
            pygame.draw.rect(self.screen, DARK_GRAY, (WINDOW_WIDTH // 2 - 2, y, 4, 10))
        
        # Draw skeletal creature
        self.creature.draw(self.screen)
    
    def draw_game(self):
        """Draw game screen"""
        # Apply screen shake
        shake_x = random.randint(-self.screen_shake, self.screen_shake) if self.screen_shake > 0 else 0
        shake_y = random.randint(-self.screen_shake, self.screen_shake) if self.screen_shake > 0 else 0
        
        # Save current display
        temp_screen = self.screen.copy()
        self.screen.fill(BLACK)
        self.screen.blit(temp_screen, (shake_x, shake_y))
        
        # Draw background
        self.draw_background()
        
        # Draw game objects
        self.left_paddle.draw(self.screen)
        self.right_paddle.draw(self.screen)
        
        for ball in self.balls:
            ball.draw(self.screen)
        
        for power_up in self.power_ups:
            power_up.draw(self.screen)
        
        # Draw particles
        for particle in self.particles:
            particle.draw(self.screen)
        
        # Draw scores
        left_score_text = self.font_large.render(str(self.left_score), True, WHITE)
        right_score_text = self.font_large.render(str(self.right_score), True, WHITE)
        
        self.screen.blit(left_score_text, (WINDOW_WIDTH // 4 - 20, 50))
        self.screen.blit(right_score_text, (3 * WINDOW_WIDTH // 4 - 20, 50))
        
        # Draw slow motion indicator
        if self.slow_motion_timer > 0:
            slow_text = self.font_small.render("SLOW MOTION", True, CYAN)
            text_rect = slow_text.get_rect(center=(WINDOW_WIDTH // 2, 100))
            self.screen.blit(slow_text, text_rect)
        
        # Draw controls
        if self.game_mode == GameMode.SINGLE_PLAYER:
            controls = self.font_tiny.render("W/S: Move Paddle | ESC: Pause", True, GRAY)
        elif self.game_mode == GameMode.TWO_PLAYER:
            controls = self.font_tiny.render("P1: W/S | P2: ↑/↓ | ESC: Pause", True, GRAY)
        else:
            controls = self.font_tiny.render("SURVIVAL MODE | ESC: Pause", True, GRAY)
        
        self.screen.blit(controls, (10, WINDOW_HEIGHT - 25))
    
    def draw_menu(self):
        """Draw main menu"""
        # Animated background
        for i in range(0, WINDOW_WIDTH, 50):
            for j in range(0, WINDOW_HEIGHT, 50):
                alpha = int(50 * (1 + math.sin(time.time() + i * 0.01 + j * 0.01)) / 2)
                color = (*DARK_GRAY, alpha)
                dot_surface = pygame.Surface((2, 2), pygame.SRCALPHA)
                dot_surface.fill(color)
                self.screen.blit(dot_surface, (i, j))
        
        # Title
        title_y = 150 + math.sin(time.time() * 2) * 10
        title_text = self.font_large.render("ENHANCED PONG", True, WHITE)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        
        # Title glow
        for offset in range(3, 0, -1):
            glow_text = self.font_large.render("ENHANCED PONG", True, BLUE)
            glow_rect = glow_text.get_rect(center=(WINDOW_WIDTH // 2 + offset, title_y + offset))
            self.screen.blit(glow_text, glow_rect)
        
        self.screen.blit(title_text, title_rect)
        
        # Menu options
        menu_options = [
            "Single Player",
            "Two Player", 
            "Survival Mode",
            "Exit"
        ]
        
        for i, option in enumerate(menu_options):
            y_pos = 300 + i * 60
            
            # Highlight selected option
            if i == self.menu_selection:
                highlight_rect = pygame.Rect(WINDOW_WIDTH // 2 - 150, y_pos - 15, 300, 40)
                pygame.draw.rect(self.screen, BLUE, highlight_rect, 2)
                color = YELLOW
            else:
                color = WHITE
            
            option_text = self.font_medium.render(option, True, color)
            option_rect = option_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos))
            self.screen.blit(option_text, option_rect)
        
        # Instructions
        instructions = [
            "Use ↑↓ to navigate, ENTER to select",
            "Collect power-ups during gameplay!"
        ]
        
        for i, instruction in enumerate(instructions):
            inst_text = self.font_tiny.render(instruction, True, GRAY)
            inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 550 + i * 25))
            self.screen.blit(inst_text, inst_rect)
    
    def draw_pause(self):
        """Draw pause screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(150)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Pause text
        pause_text = self.font_large.render("PAUSED", True, YELLOW)
        pause_rect = pause_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
        self.screen.blit(pause_text, pause_rect)
        
        # Instructions
        continue_text = self.font_small.render("Press SPACE to continue", True, WHITE)
        continue_rect = continue_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 60))
        self.screen.blit(continue_text, continue_rect)
        
        menu_text = self.font_small.render("Press ESC for main menu", True, WHITE)
        menu_rect = menu_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 90))
        self.screen.blit(menu_text, menu_rect)
    
    def draw_game_over(self):
        """Draw game over screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(200)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Determine winner
        if self.left_score > self.right_score:
            winner_text = "PLAYER 1 WINS!" if self.game_mode == GameMode.TWO_PLAYER else "YOU WIN!"
            color = GREEN
        else:
            winner_text = "PLAYER 2 WINS!" if self.game_mode == GameMode.TWO_PLAYER else "AI WINS!"
            color = RED
        
        # Winner text
        winner_surface = self.font_large.render(winner_text, True, color)
        winner_rect = winner_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50))
        self.screen.blit(winner_surface, winner_rect)
        
        # Final score
        score_text = f"{self.left_score} - {self.right_score}"
        score_surface = self.font_medium.render(score_text, True, WHITE)
        score_rect = score_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
        self.screen.blit(score_surface, score_rect)
        
        # Instructions
        instructions = [
            "Press SPACE to play again",
            "Press ESC for main menu"
        ]
        
        for i, instruction in enumerate(instructions):
            inst_surface = self.font_small.render(instruction, True, GRAY)
            inst_rect = inst_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 80 + i * 30))
            self.screen.blit(inst_surface, inst_rect)
    
    def draw(self):
        """Main draw function"""
        if self.game_mode == GameMode.MENU:
            self.screen.fill(BLACK)
            self.draw_menu()
        elif self.game_mode in [GameMode.SINGLE_PLAYER, GameMode.TWO_PLAYER, GameMode.SURVIVAL]:
            self.draw_game()
            if self.game_mode == GameMode.PAUSED:
                self.draw_pause()
        elif self.game_mode == GameMode.GAME_OVER:
            self.draw_game()
            self.draw_game_over()
        
        pygame.display.flip()
    
    def run(self):
        """Main game loop"""
        running = True
        
        while running:
            running = self.handle_events()
            self.update_game()
            self.draw()
            self.clock.tick(FPS)
        
        pygame.quit()
        sys.exit()

def main():
    print("🏓 Enhanced Pong - Ultimate Edition")
    print("=" * 40)
    print("Features:")
    print("• Classic Pong gameplay with enhancements")
    print("• Beautiful visual effects and particles")
    print("• Skeletal creature decoration (inspired by your image)")
    print("• Multiple game modes:")
    print("  - Single Player vs AI")
    print("  - Two Player local multiplayer")  
    print("  - Survival mode (harder AI)")
    print("• Power-ups system:")
    print("  - Speed Boost ⚡")
    print("  - Big/Small Paddle ⬆⬇")
    print("  - Multi-Ball ●")
    print("  - Slow Motion 🕐")
    print("• Enhanced audio and visual feedback")
    print("• Smooth animations and trail effects")
    print("• Screen shake and particle explosions")
    print("\nControls:")
    print("• W/S - Move left paddle")
    print("• ↑/↓ - Move right paddle (two player)")
    print("• SPACE - Pause/Resume/Restart")
    print("• ESC - Menu/Quit")
    print("• Arrow keys - Navigate menu")
    print("• ENTER - Select menu option")
    print("\nStarting Enhanced Pong...")
    
    try:
        game = EnhancedPongGame()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()